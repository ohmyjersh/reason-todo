[%bs.raw {|require('./index.css')|}];

external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"];

ReactDOMRe.renderToElementWithId <Todo /> "root";

register_service_worker ();