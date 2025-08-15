;; TODO: utility
(fn shuffle [arr]
  "Fisher-Yates shuffle"
  (let [n (length arr)]
    (for [i n 2 -1]
      (let [j (math.random 1 i)
            tmp (. arr i)]
        (tset arr i (. arr j))
        (tset arr j tmp))))
  arr)

;; TODO: create macros file and import it
(fn inc [n] (+ n 1))
(fn dec [n] (- n 1))

(local game-modes {:easy {:bombs 6 :cols 4 :rows 8}
                   :medium {:bombs 12 :cols 6 :rows 12}
                   :hard {:bombs 24 :cols 8 :rows 16}})

(fn valid-pos? [mode col row]
  "Checks if the given column and row are valid for the game mode."
  (and (>= col 1) (<= col mode.cols) (>= row 1) (<= row mode.rows)))

(fn pos->cell [mode cells col row]
  "Returns the cell at the given column and row."
  (. cells (+ (* (dec row) mode.cols) col)))

(fn neighbors [mode cells col row]
  "Returns the neighboring cells of the given column and row."
  (let [n [[(dec col) (dec row)]
           [col (dec row)]
           [(inc col) (dec row)]
           [(dec col) row]
           [(inc col) row]
           [(dec col) (inc row)]
           [col (inc row)]
           [(inc col) (inc row)]]]
    (icollect [_ [cur-col cur-row] (ipairs n)]
      (when (valid-pos? mode cur-col cur-row)
        (pos->cell mode cells cur-col cur-row)))))

(fn generate-cells [mode]
  (let [bombs (shuffle (fcollect [i 1 (* mode.cols mode.rows)]
                         (<= i mode.bombs)))]
    (icollect [i bomb (ipairs bombs)]
      {:bomb? bomb
       :flagged? false
       :revealed? false
       :adjacent-bombs 0
       :col (inc (% (dec i) mode.cols))
       :row (inc (math.floor (/ (dec i) mode.cols)))})))

(fn setup-adjacency [mode cells]
  (each [_ cell (ipairs cells)]
    (when cell.bomb?
      (let [neighbors (neighbors mode cells cell.col cell.row)]
        (each [_ neighbor (ipairs neighbors)]
          (set neighbor.adjacent-bombs (inc neighbor.adjacent-bombs)))))))

(fn new-game [mode-type]
  (let [mode (. game-modes mode-type)
        cells (generate-cells mode)]
    (setup-adjacency mode cells)
    {:mode mode :cells cells}))

(comment (new-game :easy))

(var game nil)

(fn reveal-cell [game cell]
  (set cell.revealed? true)
  (when (and (not cell.bomb?) (= 0 cell.adjacent-bombs))
    (let [neighbors (neighbors game.mode game.cells cell.col cell.row)]
      (each [_ neighbor (ipairs neighbors)]
        (when (not neighbor.revealed?)
          (reveal-cell game neighbor))))))

;; sprite stuff

(fn load-spritesheet []
  (love.graphics.setDefaultFilter "nearest" "nearest")
  (let [image (love.graphics.newImage "assets/spritesheet.png")]
    {:image image
     :tile (love.graphics.newQuad 16 0 16 16 image)
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
                           (: :scale 3 3)
                           (: :translate 32 32)))
  (love.graphics.setNewFont 12)
  (love.graphics.setColor 0 0 0 1)
  (love.graphics.setBackgroundColor 1 1 1 1))

(love.load)

(fn love.mousereleased [x y button _istouch _presses]
  (when (= button 1)
    (let [(gx gy) (scale-transform:inverseTransformPoint x y)
          col (inc (math.floor (/ gx 16)))
          row (inc (math.floor (/ gy 16)))]
      (when (valid-pos? game.mode col row)
        (reveal-cell game (pos->cell game.mode game.cells col row))
        (case (pos->cell game.mode game.cells col row)
          {:bomb? true} (print "Bomb!"))))))

(comment (let [my-cell {:bomb? false
                        :flagged? false
                        :revealed? false
                        :adjacent-bombs 3}]
           (case my-cell
             {:bomb? true} (print "Bomb!")
             {:adjacent-bombs 0} (print "Empty!")
             (where {:adjacent-bombs n} (= n 3)) (print "Number:" n))))

(fn cell-tile [cell]
  (if cell.revealed?
      (if cell.bomb?
          spritesheet.bomb-tile
          (. spritesheet :open-tiles (inc cell.adjacent-bombs)))
      spritesheet.tile))

(fn love.draw []
  (love.graphics.setCanvas canvas)
  (love.graphics.clear 1 1 1 1)
  (each [_ cell (ipairs game.cells)]
    (let [tile (cell-tile cell)]
      (love.graphics.draw spritesheet.image tile (* 16 (dec cell.col))
                          (* 16 (dec cell.row)))))
  (love.graphics.setCanvas)
  (love.graphics.setColor 1 1 1 1)
  (love.graphics.applyTransform scale-transform)
  (love.graphics.draw canvas 0 0))
