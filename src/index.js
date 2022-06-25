import $ from 'jquery';
import 'bootstrap';
import 'admin-lte/dist/js/adminlte.js';
import "admin-lte/dist/css/adminlte.css";
import "./css/OverlayScrollbars.css";
import "./css/shidashi.css";
import { registerProgressOutput } from './js/shiny-progress.js';
import { registerClipboardOutput } from './js/shiny-clipboard.js';
import { registerHighlightJS } from './js/import-highlightjs.js';
import { Shidashi } from './js/class-shidashi.js';
// import "./scss/shidashi.scss";

let shidashi;

function ensureShidashi() {
  if(shidashi) { return(shidashi); }
  shidashi = new Shidashi();
  return(shidashi);
}


let initialized = false;
function initShidashi() {
  if(initialized) {
    return( shidashi );
  }
  ensureShidashi();
  $('.content-wrapper').IFrame({
    onTabClick: (item) => {
      return item;
    },
    onTabChanged: (item) => {
      // console.log(item);
      if(item.length) {

        const re = /^tab--module-(.*)-shared_id-[a-zA-Z0-9]+$/g;
        let module_id = re.exec(item[0].id);
        module_id = module_id[1];

        shidashi._active_module = module_id;
        const data = {
          type: "active_module",
          id : module_id,
          label : item[0].innerText.trim()
        };
        shidashi.shinySetInput("@rave_action@", data, true, true);
        shidashi.notifyIframes("shinySetInput", ["@rave_action@", data, true, true]);
      }
      return item;
    },
    onTabCreated: (item) => {
      return item;
    },
    autoIframeMode: true,
    autoItemActive: true,
    autoShowNewTab: true,
    allowDuplicates: false,
    loadingScreen: false,
    useNavbarItems: false,
    scrollOffset: 0
  });
  initialized = true;
  return( shidashi );
}



function registerShidashi(shiny) {
  ensureShidashi();
  shidashi._shiny = shiny;
  shidashi._register_shiny();
  shidashi._finalize_initialization();
  shidashi.shiny_connected = true;
  shidashi.ensureShiny();

  registerProgressOutput(shiny);
  registerClipboardOutput(shiny, shidashi);


  return( shidashi );
}

export { initShidashi, registerShidashi };





/*if (window.hljs) {
  hljs.configure({languages: []});
  hljs.initHighlightingOnLoad();
  if (document.readyState && document.readyState === "complete") {
    window.setTimeout(function() { hljs.initHighlighting(); }, 0);
  }
}*/

// $(document).ready
