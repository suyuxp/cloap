// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );


// Check Notification Permission
if(Notification.permission !== 'granted'){
  Notification.requestPermission();
}


app.ports.notify.subscribe(function(msg) {
  console.log(msg);

  new Notification( msg.title, {
    body: msg.content,
    icon: 'mail.png'
  });
});