fluidPage( HTML("
    <head>
        <title>TODO supply a title</title>
        <meta charset='UTF-8'>
        <meta name='viewport' content='width=device-width, initial-scale=1.0'>
        <link rel='stylesheet' 
              type='text/css'
              media='screen'
              href='https://openseadragon.github.io/css/style.css'/>
        <script src='openseadragon/openseadragon.min.js'></script>
        <script src='Openseadragon-screenshot-master/openseadragonScreenshot.min.js'></script>
        <script src='Openseadragon-screenshot-master/FileSaver.js'></script>
        <script src='Openseadragon-screenshot-master/canvas-toBlob.js'></script>
	      <script src='openseadragonselection-master/dist/openseadragonselection.js'></script>
        <style>
            #container {
                width: 100% !important;
            }
        </style>
    </head>
    <body>
        <div class='demoarea'>
            <div class='demoheading'>
                Example Inline Configuration for Zoomify
            </div>
	    <div id='toolbarDiv' style='width: 100%; height: 50px;'>
	    </div>
	          <div id='toolbarDiv' style='width: 100%; height: 50px;'>
	          </div>
            <div id='example-inline-configuration-for-zoomify' style='width: 100%; height: 1000px;'>
            </div>
            <p>
                Supply all necessary information in the <code>tilesource</code> object. A minimal example object for this method looks like this:
            </p>            
        </div>
        <script type='text/javascript'>
            var viewer = OpenSeadragon({
                id: 'example-inline-configuration-for-zoomify',
                showNavigator: true,
                navigatorPosition: 'TOP_RIGHT',
                showFlipControl: true,
                showRotationControl: true,
                SequenceMode: true,
                ShowSequenceControl: true,
                crossOriginPolicy: 'Anonymous',
		            toolbar: 'toolbarDiv',
                tileSources: [{
                        type: 'zoomifytileservice',
                        width: 205176,
                        height: 90559,
                        tilesUrl: './SL14B0004838-A-8-1/'
                    }]
            });
            viewer.screenshot({
                showOptions: true
            });

        </script>
    </body>")
)