import System.IO
import Data.Char
import Data.Text
import System.Environment

data GUI = Slider Int Int
         | ColorPicker
         | Picture Int Int Graphic

type Program = [GUI]
data Graphic = Square Jval Jval Jval Jval Jval
             | Circ Jval Jval Jval Jval Jval

data Jval = Jgui Int
          | Jnum Int
          | Jplus Jval Jval
          | Jminus Jval Jval
          | Jmul Jval Jval
          | Jsin Jval

instance Num Jval where 
                 fromInteger i = Jnum i
                 x + y = Jplus x y
                 x - y = Jminus x y
                 x * y = Jmul x y
                
toJavascript :: Jval -> String
toJavascript jv = case jv of
                  Jgui i -> "(JGUI'S NAME).value"
                  Jnum i -> "(THAT INT)"
                  Jplus l r -> "(" + toJavascript l + ") + (" + toJavascript r + ")"
                

data GMonad a = GMonad (Program -> (a, Program))

gMonad :: (Program -> (a, Program)) -> GMonad a
gMonad f = GMonad f

--do s <- slider 0 100
--


prelude = "<!DOCTYPE html> \n<html> \n<script src=\"svgJS.js\"></script> \n<body> \n \n<div id=\"drawing\"> \n</div>"

stageObjects = "\n<div>  <input type=\"range\" id = \"slider\" min = 0 max = 10>\n  <input type=\"color\" id = \"colPicker\">"

scriptHandlers =  "\n</div> \n<script> \n  window.requestAnimFrame = (function(){ \n    return  window.requestAnimationFrame       || \n            window.webkitRequestAnimationFrame || \n            window.mozRequestAnimationFrame    || \n            function( callback ){ \n              window.setTimeout(callback, 1000/60); \n            }; \n  })(); \n  function animate() { \n    requestAnimFrame( animate ); \n    render(); \n  } \n  \n  function render() { \n    var now = (new Date().getTime()*0.001) - startTime;  "

varsAndDrawings = "\n  var rect = drawSVG.rect(100,100).attr({ fill: '#f06' })\n  var slider = document.getElementById(\"slider\")\n  var colPicker = document.getElementById(\"colPicker\")"

animBehaviors = "\n    var ticker = Math.sin(now) + 1;  \n    rect.attr('x', ticker*100) \n    var slidey = slider.value \n    rect.attr('y', slidey * 20) \n    var color = colPicker.value \n    rect.attr('fill', color)" 

midlude = "\n  } \n  \n  var drawSVG = SVG('drawing').size(300,300) \n  var startTime = new Date().getTime()*0.001; \n "

postlude = "\n  animate(); \n \n \n</script> \n \n</body> \n</html>"

baseCanvas = prelude ++ stageObjects ++ scriptHandlers ++ animBehaviors ++ midlude ++ varsAndDrawings ++ postlude


main = do
    --hitmel <- readFile "textHTML.txt"
    writeFile "properHTML.html" baseCanvas