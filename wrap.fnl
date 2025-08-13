(fn shuffle [arr]
  "Fisher-Yates shuffle"
  (let [n (length arr)]
    (for [i n 2 -1]
      (let [j (math.random 1 i)
            tmp (. arr i)]
        (tset arr i (. arr j))
        (tset arr j tmp))))
  arr)

(fn inc [n] (+ n 1))
(fn dec [n] (- n 1))

(fn xy->index [x y cols]
  (+ (* (dec y) cols) x))

(fn index->xy [index cols]
  (let [x (inc (% (dec index) cols))
        y (inc (math.floor (/ (dec index) cols)))]
    [x y]))

(fn neighbors [x y]
  [;; tl
   [(dec x) (dec y)]
   ;; t
   [x (dec y)]
   ;; tr
   [(inc x) (dec y)]
   ;; l
   [(dec x) y]
   ;; r
   [(inc x) y]
   ;; bl
   [(dec x) (inc y)]
   ;; b
   [x (inc y)]
   ;; br
   [(inc x) (inc y)]])

(local game-modes {:easy {:bombs 6 :cols 4 :rows 8}
                   :medium {:bombs 12 :cols 6 :rows 12}
                   :hard {:bombs 24 :cols 8 :rows 16}})

(fn generate-cells [bombs cols rows]
  (shuffle (fcollect [i 1 (* cols rows)]
             (if (<= i bombs)
                 {:bomb? true
                  :flagged? false
                  :revealed? false
                  :adjacent-bombs 0}
                 {:bomb? false
                  :flagged? false
                  :revealed? false
                  :adjacent-bombs 0}))))

(fn find-bombs [cells]
  (icollect [i cell (ipairs cells)] (when cell.bomb? i)))

(fn setup-adjacency [cells bombs cols rows]
  (each [_ bomb (ipairs bombs)]
    (let [[bomb-x bomb-y] (index->xy bomb cols)
          neighbors (neighbors bomb-x bomb-y)]
      (each [_ [x y] (ipairs neighbors)]
        (when (and (>= x 1) (<= x cols) (>= y 1) (<= y rows))
          (let [index (xy->index x y cols)]
            (tset cells index :adjacent-bombs
                  (inc (. cells index :adjacent-bombs)))))))))

(fn new-game [mode]
  (let [{: bombs : cols : rows} (. game-modes mode)
        cells (generate-cells bombs cols rows)
        bombs (find-bombs cells)]
    (setup-adjacency cells bombs cols rows)
    {:cells cells :cols cols :rows rows :bombs bombs}))

(comment (new-game :easy))

(var game nil)

;; sprite stuff

(fn load-spritesheet []
  (love.graphics.setDefaultFilter "nearest" "nearest")
  (let [image (love.graphics.newImage "assets/spritesheet.png")]
    {:image image
     :open-tiles (fcollect [i 0 9]
                   (love.graphics.newQuad (+ 80 (* i 16)) 0 16 16 image))
     :bomb-tile (love.graphics.newQuad 80 16 16 16 image)}))

(local spritesheet (load-spritesheet))

(local canvas (love.graphics.newCanvas 256 256))
(var scale-transform nil)

;; love callbacks

(fn love.load []
  (print "Loading game...")
  (set game (new-game :hard))
  (set scale-transform (-> (love.math.newTransform)
                           (: :scale 3 3)))
  (love.graphics.setNewFont 12)
  (love.graphics.setColor 0 0 0 1)
  (love.graphics.setBackgroundColor 1 1 1 1))

(love.load)

(fn love.draw []
  (love.graphics.setCanvas canvas)
  (love.graphics.clear 1 1 1 1)
  (each [i cell (ipairs game.cells)]
    (let [[x y] (index->xy i game.cols)
          tile (if cell.bomb?
                   spritesheet.bomb-tile
                   (. spritesheet :open-tiles (inc cell.adjacent-bombs)))]
      (love.graphics.draw spritesheet.image tile (* 16 (dec x)) (* 16 (dec y)))))
  (love.graphics.setCanvas)
  (love.graphics.setColor 1 1 1 1)
  (love.graphics.applyTransform scale-transform)
  (love.graphics.draw canvas 0 0))
