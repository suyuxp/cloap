// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

app.ports.check.subscribe(function(word) {
  if(Notification.permission !== 'granted'){
    Notification.requestPermission();
  }

  new Notification( "Hello", {
    body: "This is a test",
    icon : "star.ico"
  });

  var suggestions = ["abc", "def", "xyz", "123"];
  app.ports.suggestions.send(suggestions);
});