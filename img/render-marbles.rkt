#lang typed/racket


(require pict3d
         typed/racket/draw)

(: sphere-bitmap (-> (U RGBA
                        Real
                        (Listof Real) (Vectorof Real) FlVector
                        String (Instance Color%))
                     (Instance Bitmap%)))
(define (sphere-bitmap sphere-color)
  (parameterize ([current-pict3d-width 100]
                 [current-pict3d-height 100]
                 [current-pict3d-background  (rgba "white" 100)]
                 [current-color  (rgba sphere-color)])
    (pict3d->bitmap
     (combine (sphere origin 1/8)
              (light (pos 0 1 1) (emitted "white" 2))))))


(send (sphere-bitmap "green") save-file "green-marble.png" 'png)
(send (sphere-bitmap "yellow") save-file "yellow-marble.png" 'png)
(send (sphere-bitmap "red") save-file "red-marble.png" 'png)
(send (sphere-bitmap "cornflowerblue") save-file "blue-marble.png" 'png)
