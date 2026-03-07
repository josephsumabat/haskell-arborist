{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Scripts.Playground (runPlayground) where

import AST qualified
import AST.Extension ()
import AST.Haskell
import AST.Haskell.Generated () -- for HasField instances
import Arborist.ProgramIndex (ProgramIndex, prgsToMap)
import Arborist.Renamer
import Arborist.Scope.Types qualified as Scope
import Arborist.Scope.Types
  ( GlblConstructorInfo (..)
  , GlblNameInfo (..)
  , GlblVarInfo (..)
  , NameKind
  , ResolvedVarInfo (..)
  , resolvedLclVarToLoc
  )
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (FromJSON, Value, encode, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.HashMap.Lazy qualified as Map
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.List.NonEmpty qualified as NE
import Data.Pos (Pos (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import HaskellAnalyzer (parsePrg)
import Hir.Types (ModuleText (..))
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import System.Process (spawnProcess)
import Text.RawString.QQ (r)

defaultPort :: Int
defaultPort = 8080

runPlayground :: IO ()
runPlayground = do
  let port = defaultPort
      url = "http://localhost:" <> show port
  putStrLn $ "Starting Arborist playground at " <> url
  openBrowser url
  run port app

openBrowser :: Prelude.String -> IO ()
openBrowser url = do
  result <- try @SomeException $ spawnProcess "xdg-open" [url]
  case result of
    Right _ -> return ()
    Left _ -> void $ try @SomeException $ spawnProcess "open" [url]

-- ── Request type ──────────────────────────────────────────────────────────────

data RenameRequest = RenameRequest
  { modules :: [T.Text] -- source of each module
  , active :: Int -- index into modules list to rename
  }
  deriving (Generic, FromJSON)

-- ── Server ────────────────────────────────────────────────────────────────────

app :: Application
app req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", []) ->
      respond $
        responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] indexHtml
    ("POST", ["rename"]) -> do
      body <- strictRequestBody req
      let result = case Aeson.decode body of
            Nothing ->
              object ["error" .= ("invalid JSON request" :: T.Text)]
            Just (RenameRequest {modules = srcs, active = idx})
              | idx < 0 || idx >= length srcs ->
                  object ["error" .= ("active index out of range" :: T.Text)]
              | otherwise ->
                  renameMultiToJson srcs idx
      respond $
        responseLBS
          status200
          [("Content-Type", "application/json; charset=utf-8")]
          (encode result)
    _ ->
      respond $ responseLBS status404 [] "Not found"

-- ── AST → JSON ────────────────────────────────────────────────────────────────

renameMultiToJson :: [T.Text] -> Int -> Value
renameMultiToJson srcs activeIdx =
  let parsedMods = map parsePrg srcs
      programIndex :: ProgramIndex
      programIndex = prgsToMap (map snd parsedMods)
      (_, activePrg) = parsedMods !! activeIdx
      exportIndex = Map.empty
   in case renamePrg programIndex exportIndex activePrg of
        Nothing -> object ["error" .= ("failed to rename AST" :: T.Text)]
        Just ast -> nodeToJson ast.dynNode

nodeToJson :: AST.DynNode -> Value
nodeToJson node =
  object
    [ "type" .= node.nodeType
    , "text" .= node.nodeText
    , "range" .= rangeToJson node.nodeLineColRange
    , "ext" .= nodeExtJson node
    , "children" .= map nodeToJson node.nodeChildren
    ]

rangeToJson :: LineColRange -> Value
rangeToJson (UnsafeLineColRange s e) =
  toJSON [s.line.pos, s.col.pos, e.line.pos, e.col.pos]

modName :: ModuleText -> T.Text
modName (ModuleText {text = t, parts = _}) = t

type ExtTypes =
  Variable RenamePhase
    AST.:+ Name RenamePhase
    AST.:+ Constructor RenamePhase
    AST.:+ AST.Nil

nodeExtJson :: AST.DynNode -> Value
nodeExtJson node =
  case AST.cast @ExtTypes node of
    Just (AST.Inj @(Variable RenamePhase) n) -> maybe (toJSON (Nothing :: Maybe ())) resolvedVarJson n.ext
    Just (AST.Inj @(Name RenamePhase) n) -> maybe (toJSON (Nothing :: Maybe ())) resolvedNameJson n.ext
    Just (AST.Inj @(Constructor RenamePhase) n) -> maybe (toJSON (Nothing :: Maybe ())) resolvedConJson n.ext
    _ -> toJSON (Nothing :: Maybe ())

resolvedVarJson :: ResolvedVariable -> Value
resolvedVarJson rv =
  case rv of
    ResolvedVariable (ResolvedGlobal (GlblVarInfo {originatingMod = om, loc = dl})) ->
      object
        [ "tag" .= ("Variable" :: T.Text)
        , "status" .= ("ResolvedGlobal" :: T.Text)
        , "defMod" .= modName om
        , "defRange" .= rangeToJson dl
        ]
    ResolvedVariable (ResolvedLocal l) ->
      object
        [ "tag" .= ("Variable" :: T.Text)
        , "status" .= ("ResolvedLocal" :: T.Text)
        , "defRange" .= rangeToJson (resolvedLclVarToLoc l)
        ]
    AmbiguousGlobalVar gs ->
      object
        [ "tag" .= ("Variable" :: T.Text)
        , "status" .= ("Ambiguous" :: T.Text)
        , "candidates" .= map (\(GlblVarInfo {originatingMod = om}) -> modName om) (NE.toList gs)
        ]
    AmbiguousLocalVar _ ->
      object ["tag" .= ("Variable" :: T.Text), "status" .= ("AmbiguousLocal" :: T.Text)]
    ResolvedField ->
      object ["tag" .= ("Variable" :: T.Text), "status" .= ("Field" :: T.Text)]
    NoVarFound ->
      object ["tag" .= ("Variable" :: T.Text), "status" .= ("Unresolved" :: T.Text)]

resolvedNameJson :: ResolvedName -> Value
resolvedNameJson rn =
  case rn of
    ResolvedName (GlblNameInfo {originatingMod = om, loc = dl, nameKind = nk}) _ ->
      object
        [ "tag" .= ("Name" :: T.Text)
        , "status" .= ("Resolved" :: T.Text)
        , "defMod" .= modName om
        , "defRange" .= rangeToJson dl
        , "nameKind" .= nameKindStr nk
        ]
    AmbiguousName ns ->
      object
        [ "tag" .= ("Name" :: T.Text)
        , "status" .= ("Ambiguous" :: T.Text)
        , "candidates" .= map (\(GlblNameInfo {originatingMod = om}) -> modName om) (NE.toList ns)
        ]
    NoNameFound ->
      object ["tag" .= ("Name" :: T.Text), "status" .= ("Unresolved" :: T.Text)]

resolvedConJson :: ResolvedConstructor -> Value
resolvedConJson rc =
  case rc of
    ResolvedConstructor (GlblConstructorInfo {originatingMod = om, loc = dl}) ->
      object
        [ "tag" .= ("Constructor" :: T.Text)
        , "status" .= ("Resolved" :: T.Text)
        , "defMod" .= modName om
        , "defRange" .= rangeToJson dl
        ]
    AmbiguousConstructor cs ->
      object
        [ "tag" .= ("Constructor" :: T.Text)
        , "status" .= ("Ambiguous" :: T.Text)
        , "candidates" .= map (\(GlblConstructorInfo {originatingMod = om}) -> modName om) (NE.toList cs)
        ]
    NoConstructorFound ->
      object ["tag" .= ("Constructor" :: T.Text), "status" .= ("Unresolved" :: T.Text)]

nameKindStr :: NameKind -> T.Text
nameKindStr nk = case nk of
  Scope.DataDecl -> "data"
  Scope.NewtypeDecl -> "newtype"
  Scope.ClassDecl -> "class"
  Scope.TypeDecl -> "type"
  Scope.DataFamilyDecl -> "data family"
  Scope.TypeFamilyDecl -> "type family"
  Scope.TypeSynonymDecl -> "type synonym"

-- ── Embedded HTML ─────────────────────────────────────────────────────────────

indexHtml :: LBS.ByteString
indexHtml =
  LBSC.pack
    [r|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Arborist Playground</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      height: 100vh; display: flex; flex-direction: column;
      font-family: 'Consolas', 'Monaco', 'Menlo', monospace;
      background: #1e1e1e; color: #d4d4d4;
    }
    header {
      padding: 8px 20px; background: #252526;
      border-bottom: 1px solid #3d3d3d;
      display: flex; align-items: center; gap: 16px; flex-shrink: 0;
    }
    header h1 { font-size: 14px; font-weight: 600; color: #d4d4d4; }
    header .sub { font-size: 11px; color: #666; }
    .panels { display: flex; flex: 1; overflow: hidden; }
    .panel { flex: 1; display: flex; flex-direction: column; overflow: hidden; min-width: 0; }
    .panel + .panel { border-left: 1px solid #3d3d3d; }

    /* ── tabs ── */
    .tab-bar {
      display: flex; align-items: stretch;
      background: #2d2d2d; border-bottom: 1px solid #3d3d3d;
      overflow-x: auto; flex-shrink: 0; min-height: 32px;
    }
    .tab-bar::-webkit-scrollbar { height: 3px; }
    .tab-bar::-webkit-scrollbar-thumb { background: #555; }
    .tab {
      display: flex; align-items: center; gap: 6px;
      padding: 0 10px; min-width: 80px; max-width: 180px;
      font-size: 12px; cursor: pointer; user-select: none;
      border-right: 1px solid #3d3d3d; white-space: nowrap;
      color: #888; flex-shrink: 0;
    }
    .tab.active { background: #1e1e1e; color: #d4d4d4; border-bottom: 2px solid #007acc; }
    .tab:hover:not(.active) { background: #252526; color: #ccc; }
    .tab-name { flex: 1; overflow: hidden; text-overflow: ellipsis; }
    .tab-close {
      color: #555; font-size: 14px; line-height: 1;
      padding: 1px 2px; border-radius: 2px;
    }
    .tab-close:hover { color: #d4d4d4; background: #555; }
    .tab-add {
      padding: 0 12px; font-size: 18px; color: #555; cursor: pointer;
      display: flex; align-items: center;
    }
    .tab-add:hover { color: #d4d4d4; }

    /* ── editor ── */
    .editor-wrap { flex: 1; position: relative; overflow: hidden; }
    #source {
      position: absolute; inset: 0;
      background: #1e1e1e; color: #d4d4d4;
      border: none; outline: none; padding: 14px;
      font-family: inherit; font-size: 13px; line-height: 1.6;
      resize: none; tab-size: 2; width: 100%; height: 100%;
      caret-color: #d4d4d4;
    }
    #hl-overlay {
      position: absolute; inset: 0; pointer-events: none;
      padding: 14px; font-family: inherit; font-size: 13px; line-height: 1.6;
      white-space: pre-wrap; word-break: break-all;
      color: transparent; overflow: hidden;
    }
    #hl-overlay .hl     { background: rgba(255,200,0,0.18); border-radius: 2px; }
    #hl-overlay .hl-def { background: rgba(100,200,255,0.22); border-radius: 2px; }

    /* ── AST panel ── */
    .panel-title {
      padding: 5px 14px; background: #2d2d2d;
      font-size: 11px; color: #888; text-transform: uppercase;
      letter-spacing: 0.8px; border-bottom: 1px solid #3d3d3d; flex-shrink: 0;
    }
    #ast { flex: 1; overflow: auto; padding: 8px 4px; font-size: 12px; }

    /* ── nodes ── */
    .node { padding-left: 16px; }
    .node-row {
      display: flex; align-items: baseline; gap: 6px;
      padding: 1px 4px; border-radius: 3px; cursor: pointer; white-space: nowrap;
    }
    .node-row:hover  { background: #2a2d2e; }
    .node-row.active { background: #37373d; outline: 1px solid #5a5a5a; }
    .node-type  { color: #9cdcfe; font-weight: 500; }
    .node-text  { color: #ce9178; max-width: 200px; overflow: hidden; text-overflow: ellipsis; }
    .node-loc   { color: #555; font-size: 10px; }
    .node-info  { display: flex; align-items: center; gap: 4px; margin-left: 2px; }

    /* resolution badge */
    .badge {
      font-size: 10px; padding: 1px 5px; border-radius: 3px;
      cursor: default; white-space: nowrap;
    }
    .badge.clickable { cursor: pointer; }
    .badge-local    { background: #264f78; color: #9cdcfe; }
    .badge-global   { background: #1e3a1e; color: #6ddc6d; }
    .badge-ambig    { background: #4a3000; color: #ffcc00; }
    .badge-unresolv { background: #2a2a2a; color: #555; }
    .badge-field    { background: #2a2a2a; color: #777; }
    .badge-kind     { background: #3a2a50; color: #b08fe0; font-size: 9px; }
    .def-loc { color: #4ec9b0; font-size: 10px; }
    .def-loc.clickable { cursor: pointer; text-decoration: underline dotted; }

    .toggle { color: #555; user-select: none; width: 12px; display: inline-block; flex-shrink: 0; }
    .node-children.collapsed { display: none; }
  </style>
</head>
<body>
  <header>
    <h1>Arborist Playground</h1>
    <span class="sub">click nodes to select · click local badge to jump to definition</span>
  </header>
  <div class="panels">
    <!-- left: editor with tabs -->
    <div class="panel">
      <div class="tab-bar" id="tab-bar">
        <!-- tabs injected by JS -->
        <div class="tab-add" id="tab-add" title="Add module">+</div>
      </div>
      <div class="editor-wrap">
        <div id="hl-overlay" aria-hidden="true"></div>
        <textarea id="source" spellcheck="false"></textarea>
      </div>
    </div>
    <!-- right: AST -->
    <div class="panel">
      <div class="panel-title">Renamed AST</div>
      <div id="ast"><span style="color:#666;padding:14px;display:block">Loading&#x2026;</span></div>
    </div>
  </div>

<script>
// ── state ─────────────────────────────────────────────────────────────────────
var modules = [
  { name: 'Main', src: 'module Main where\n\ngreet :: String -> String\ngreet name = "Hello, " ++ name ++ "!"\n\nmain :: IO ()\nmain = putStrLn (greet "World")\n' },
];
var activeIdx   = 0;
var tree        = null;
var activeRow   = null;
var debTimer    = null;
var curUseRange = null;
var curDefRange = null;

var srcEl  = document.getElementById('source');
var astEl  = document.getElementById('ast');
var hlEl   = document.getElementById('hl-overlay');
var tabBar = document.getElementById('tab-bar');
var addBtn = document.getElementById('tab-add');

// ── tab management ────────────────────────────────────────────────────────────
function renderTabs() {
  // remove all existing tab elements (not the add button)
  Array.from(tabBar.querySelectorAll('.tab')).forEach(function(t) { t.remove(); });

  modules.forEach(function(mod, i) {
    var tab = document.createElement('div');
    tab.className = 'tab' + (i === activeIdx ? ' active' : '');

    var nameEl = document.createElement('span');
    nameEl.className = 'tab-name';
    nameEl.textContent = mod.name;
    // double-click to rename
    nameEl.addEventListener('dblclick', function(e) {
      e.stopPropagation();
      var newName = prompt('Module name:', mod.name);
      if (newName && newName.trim()) {
        modules[i].name = newName.trim();
        renderTabs();
        fetchAndRender();
      }
    });
    tab.appendChild(nameEl);

    var closeBtn = document.createElement('span');
    closeBtn.className = 'tab-close';
    closeBtn.textContent = '\u00D7';
    closeBtn.title = 'Close module';
    closeBtn.addEventListener('click', function(e) {
      e.stopPropagation();
      if (modules.length === 1) return; // keep at least one
      modules.splice(i, 1);
      if (activeIdx >= modules.length) activeIdx = modules.length - 1;
      renderTabs();
      srcEl.value = modules[activeIdx].src;
      fetchAndRender();
    });
    tab.appendChild(closeBtn);

    tab.addEventListener('click', function() {
      if (activeIdx === i) return;
      modules[activeIdx].src = srcEl.value; // save current
      activeIdx = i;
      srcEl.value = modules[i].src;
      renderTabs();
      curUseRange = null; curDefRange = null;
      hlEl.textContent = '';
      fetchAndRender();
    });

    tabBar.insertBefore(tab, addBtn);
  });
}

addBtn.addEventListener('click', function() {
  modules[activeIdx].src = srcEl.value;
  var n = modules.length + 1;
  modules.push({ name: 'Module' + n, src: 'module Module' + n + ' where\n\n' });
  activeIdx = modules.length - 1;
  renderTabs();
  srcEl.value = modules[activeIdx].src;
  fetchAndRender();
});

// ── editor events ─────────────────────────────────────────────────────────────
srcEl.addEventListener('scroll', function() {
  hlEl.scrollTop  = srcEl.scrollTop;
  hlEl.scrollLeft = srcEl.scrollLeft;
});

srcEl.addEventListener('keydown', function(e) {
  if (e.key === 'Tab') {
    e.preventDefault();
    var s = srcEl.selectionStart, end = srcEl.selectionEnd;
    srcEl.value = srcEl.value.substring(0, s) + '  ' + srcEl.value.substring(end);
    srcEl.selectionStart = srcEl.selectionEnd = s + 2;
  }
});

srcEl.addEventListener('input', function() {
  clearTimeout(debTimer);
  debTimer = setTimeout(fetchAndRender, 300);
});

srcEl.addEventListener('keyup',   onCursorMove);
srcEl.addEventListener('mouseup', onCursorMove);
srcEl.addEventListener('click',   onCursorMove);

function onCursorMove() {
  if (!tree) return;
  var lc   = offsetToLC(srcEl.value, srcEl.selectionStart);
  var node = deepestAt(tree, lc[0], lc[1]);
  if (node && node._row) activateNode(node._row, node.range, null);
}

// ── fetch ─────────────────────────────────────────────────────────────────────
function fetchAndRender() {
  modules[activeIdx].src = srcEl.value;
  return (async function() {
    try {
      var resp = await fetch('/rename', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ modules: modules.map(function(m) { return m.src; }), active: activeIdx })
      });
      var json = await resp.json();
      if (json.error) {
        astEl.innerHTML = '<span style="color:#f48771;padding:14px;display:block">' + esc(json.error) + '</span>';
        tree = null; return;
      }
      tree = json;
      astEl.innerHTML = '';
      astEl.appendChild(renderNode(json));
      curUseRange = null; curDefRange = null;
      onCursorMove();
    } catch(e) {
      astEl.innerHTML = '<span style="color:#f48771;padding:14px;display:block">Error: ' + esc(String(e.message)) + '</span>';
    }
  })();
}

// ── render AST node ───────────────────────────────────────────────────────────
function renderNode(node) {
  var wrap = document.createElement('div');
  wrap.className = 'node';

  var row = document.createElement('div');
  row.className = 'node-row';
  node._row = row;

  var hasKids = node.children && node.children.length > 0;

  var tog = document.createElement('span');
  tog.className = 'toggle';
  tog.textContent = hasKids ? '\u25B6' : ' ';
  row.appendChild(tog);

  var typeSpan = document.createElement('span');
  typeSpan.className = 'node-type';
  typeSpan.textContent = node.type;
  row.appendChild(typeSpan);

  if (node.text && (!hasKids || node.text.length < 40)) {
    var txt = document.createElement('span');
    txt.className = 'node-text';
    txt.textContent = JSON.stringify(node.text);
    row.appendChild(txt);
  }

  // location of this node
  if (node.range) {
    var locSpan = document.createElement('span');
    locSpan.className = 'node-loc';
    locSpan.textContent = (node.range[0]+1) + ':' + (node.range[1]+1);
    row.appendChild(locSpan);
  }

  // resolution info
  var info = buildInfo(node.ext);
  if (info) row.appendChild(info);

  row.addEventListener('click', function(e) {
    e.stopPropagation();
    selectRange(node.range);
    activateNode(row, node.range, curDefRange);
  });

  wrap.appendChild(row);

  if (hasKids) {
    var kids = document.createElement('div');
    kids.className = 'node-children collapsed';
    node.children.forEach(function(c) { kids.appendChild(renderNode(c)); });
    wrap.appendChild(kids);

    tog.addEventListener('click', function(e) {
      e.stopPropagation();
      var closed = kids.classList.toggle('collapsed');
      tog.textContent = closed ? '\u25B6' : '\u25BC';
    });
  }
  return wrap;
}

function buildInfo(ext) {
  if (!ext || !ext.tag) return null;
  var wrap = document.createElement('span');
  wrap.className = 'node-info';

  // nameKind badge (for types/classes)
  if (ext.nameKind) {
    var kb = document.createElement('span');
    kb.className = 'badge badge-kind';
    kb.textContent = ext.nameKind;
    wrap.appendChild(kb);
  }

  // resolution badge + def location
  var badge = document.createElement('span');
  badge.className = 'badge';

  switch (ext.status) {
    case 'ResolvedLocal': {
      badge.className += ' badge-local';
      badge.textContent = 'local';
      if (ext.defRange) {
        badge.className += ' clickable';
        badge.title = 'Click to jump to definition';
        badge.addEventListener('click', function(e) {
          e.stopPropagation();
          jumpToRange(ext.defRange, null);
        });
        var dl = document.createElement('span');
        dl.className = 'def-loc clickable';
        dl.textContent = '\u2192 ' + (ext.defRange[0]+1) + ':' + (ext.defRange[1]+1);
        dl.title = 'Jump to definition';
        dl.addEventListener('click', function(e) {
          e.stopPropagation();
          jumpToRange(ext.defRange, null);
        });
        wrap.appendChild(badge);
        wrap.appendChild(dl);
        return wrap;
      }
      break;
    }
    case 'ResolvedGlobal': case 'Resolved': {
      badge.className += ' badge-global';
      badge.textContent = shortMod(ext.defMod);
      badge.title = 'Defined in ' + (ext.defMod || '?');
      if (ext.defRange) {
        badge.className += ' clickable';
        badge.title += ' \u2014 click to jump';
        badge.addEventListener('click', function(e) {
          e.stopPropagation();
          jumpToRange(ext.defRange, ext.defMod);
        });
        var dl2 = document.createElement('span');
        dl2.className = 'def-loc clickable';
        dl2.textContent = (ext.defRange[0]+1) + ':' + (ext.defRange[1]+1);
        dl2.title = 'Jump to definition';
        dl2.addEventListener('click', function(e) {
          e.stopPropagation();
          jumpToRange(ext.defRange, ext.defMod);
        });
        wrap.appendChild(badge);
        wrap.appendChild(dl2);
        return wrap;
      }
      break;
    }
    case 'Ambiguous': {
      badge.className += ' badge-ambig';
      badge.textContent = 'ambiguous';
      badge.title = (ext.candidates || []).join(', ');
      break;
    }
    case 'Field': {
      badge.className += ' badge-field';
      badge.textContent = 'field';
      break;
    }
    case 'Unresolved': case 'AmbiguousLocal': {
      badge.className += ' badge-unresolv';
      badge.textContent = '?';
      break;
    }
    default: return null;
  }

  wrap.appendChild(badge);
  return wrap;
}

function shortMod(mod) {
  if (!mod) return 'global';
  var parts = mod.split('.');
  return parts[parts.length - 1];
}

// Jump to defRange, optionally switching to the tab whose name matches modName.
function jumpToRange(defRange, modName) {
  if (!defRange || defRange.length < 4) return;
  // Try to find a matching tab by module name
  if (modName) {
    var idx = modules.findIndex(function(m) { return m.name === modName; });
    if (idx !== -1 && idx !== activeIdx) {
      modules[activeIdx].src = srcEl.value;
      activeIdx = idx;
      srcEl.value = modules[activeIdx].src;
      renderTabs();
      // Render the new tab's AST then jump after it finishes
      fetchAndRender().then(function() { selectRange(defRange); });
      return;
    }
  }
  selectRange(defRange);
  renderOverlay(null, defRange);
}

// ── highlights ────────────────────────────────────────────────────────────────
function activateNode(rowEl, useRange, defRange) {
  if (activeRow) activeRow.classList.remove('active');
  activeRow = rowEl;
  if (rowEl) {
    rowEl.classList.add('active');
    expandTo(rowEl);
    rowEl.scrollIntoView({ block: 'nearest' });
  }
  curUseRange = useRange;
  curDefRange = defRange;
  renderOverlay(useRange, defRange);
}

function renderOverlay(useRange, defRange) {
  var text = srcEl.value;
  var spans = [];
  if (useRange) {
    var s  = lcToOffset(text, useRange[0], useRange[1]);
    var e  = lcToOffset(text, useRange[2], useRange[3]);
    if (s < e) spans.push([s, e, 'hl']);
  }
  if (defRange) {
    var s2 = lcToOffset(text, defRange[0], defRange[1]);
    var e2 = lcToOffset(text, defRange[2], defRange[3]);
    if (s2 < e2) spans.push([s2, e2, 'hl-def']);
  }
  if (!spans.length) { hlEl.textContent = ''; return; }
  spans.sort(function(a, b) { return a[0] - b[0]; });
  var html = '', pos = 0;
  spans.forEach(function(sp) {
    html += esc(text.slice(pos, sp[0]));
    html += '<span class="' + sp[2] + '">' + esc(text.slice(sp[0], sp[1])) + '</span>';
    pos = sp[1];
  });
  html += esc(text.slice(pos));
  hlEl.innerHTML = html;
  hlEl.scrollTop = srcEl.scrollTop;
}

function selectRange(range) {
  if (!range || range.length < 4) return;
  var text = srcEl.value;
  var s = lcToOffset(text, range[0], range[1]);
  var e = lcToOffset(text, range[2], range[3]);
  srcEl.focus();
  srcEl.setSelectionRange(s, e);
  var lh = parseFloat(getComputedStyle(srcEl).lineHeight) || 20;
  srcEl.scrollTop = Math.max(0, range[0] * lh - srcEl.clientHeight / 3);
}

// ── tree traversal ────────────────────────────────────────────────────────────
function deepestAt(node, line, col) {
  var r = node.range;
  if (!r) return null;
  if (line < r[0] || line > r[2]) return null;
  if (line === r[0] && col < r[1]) return null;
  if (line === r[2] && col >= r[3]) return null;
  if (node.children) {
    for (var i = 0; i < node.children.length; i++) {
      var hit = deepestAt(node.children[i], line, col);
      if (hit) return hit;
    }
  }
  return node;
}

function expandTo(el) {
  var p = el.parentElement;
  while (p && p !== astEl) {
    if (p.classList.contains('node-children') && p.classList.contains('collapsed')) {
      p.classList.remove('collapsed');
      var sib = p.previousElementSibling;
      var tog = sib && sib.querySelector('.toggle');
      if (tog) tog.textContent = '\u25BC';
    }
    p = p.parentElement;
  }
}

// ── utils ─────────────────────────────────────────────────────────────────────
function offsetToLC(text, offset) {
  var line = 0, col = 0;
  for (var i = 0; i < offset && i < text.length; i++) {
    if (text[i] === '\n') { line++; col = 0; } else { col++; }
  }
  return [line, col];
}

function lcToOffset(text, line, col) {
  var l = 0, c = 0;
  for (var i = 0; i < text.length; i++) {
    if (l === line && c === col) return i;
    if (text[i] === '\n') { l++; c = 0; } else { c++; }
  }
  return text.length;
}

function esc(s) {
  return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

// ── init ──────────────────────────────────────────────────────────────────────
renderTabs();
srcEl.value = modules[0].src;
fetchAndRender();
</script>
</body>
</html>
|]
