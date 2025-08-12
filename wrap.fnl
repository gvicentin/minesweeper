(local num-cols 4)
(local num-rows 8)
(local num-bombs 6)

(fn contains? [coll item]
  "Check if an item is in a collection."
  (accumulate [found false _ v (ipairs coll) &until found]
    (= v item)))

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

(fn xy->index [x y]
  (+ (* (dec y) num-cols) x))

(fn index->xy [index]
  (let [x (inc (% (dec index) num-cols))
        y (inc (math.floor (/ (dec index) num-cols)))]
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

(fn add-neighbors [board bomb]
  (let [[bomb-x bomb-y] (index->xy bomb)
        neighbors (neighbors bomb-x bomb-y)]
    (each [_ [x y] (ipairs neighbors)]
      (when (and (>= x 1) (<= x num-cols) (>= y 1) (<= y num-rows))
        (let [index (xy->index x y)]
          (when (not= :bomb (. board index))
            (tset board index (inc (. board index)))))))))

(local board (shuffle (fcollect [i 1 (* num-cols num-rows)]
                        (if (< i 6) :bomb 0))))

(local bombs (icollect [i v (ipairs board)] (if (= v :bomb) i)))

(each [_ bomb (ipairs bombs)]
    (add-neighbors board bomb))

(comment (contains? [1 2 3] 2)
  (contains? [1 2 3] 4)
  ;; returns zero for hash maps
  (length {:a "foo" :b "bar"})
  (shuffle ["bomb" "bomb" "bomb" 0 0 0 0 0 0 0 0 0 0 0 0 0])
  (math.random 1 2)
  (xy->index 1 2)
  (xy->index 3 2)
  (xy->index 4 4)
  (index->xy 5)
  (index->xy 7)
  (index->xy 16)
  (add-neighbors board 5)
  (each [_ bomb (ipairs bombs)]
    (add-neighbors board bomb)))

(fn load-spritesheet []
 (love.graphics.setDefaultFilter "nearest" "nearest")
 (let [image (love.graphics.newImage "assets/spritesheet.png")]
 {:image image
  :open-tiles (fcollect [i 0 9] 
                (love.graphics.newQuad (+ 80 (* i 16)) 0 16 16 image))}))

(local spritesheet (load-spritesheet))

(local canvas (love.graphics.newCanvas 256 256))
(var scale-transform nil)

(fn love.load []
  (print "Loading game...")
  (set scale-transform (-> (love.math.newTransform)
                           (: :scale 3 3)))
  (love.graphics.setNewFont 12)
  (love.graphics.setColor 0 0 0 1)
  (love.graphics.setBackgroundColor 1 1 1 1))

(love.load)

(fn love.draw []
  (love.graphics.setCanvas canvas)
  (love.graphics.clear 1 1 1 1)
  (for [j 1 4]
   (for [i 1 9]
    (love.graphics.draw spritesheet.image (. spritesheet :open-tiles i) (* 16 (dec i)) (* 16 (dec j)))))
  (love.graphics.setCanvas)
  (love.graphics.setColor 1 1 1 1)
  (love.graphics.applyTransform scale-transform)
  (love.graphics.draw canvas 0 0))
