; UVA/VU - Multi-Agent Systems
; Lecturers: T. Bosse & M.C.A. Klein
; Lab assistants: D. Formolo & L. Medeiros


; --- Assignment 4.1 - Template ---
; Please use this template as a basis for the code to generate the behaviour of your team of vacuum cleaners.
; However, feel free to extend this with any variable or method you think is necessary.


; --- Settable variables ---
; The following settable variables are given as part of the 'Interface' (hence, these variables do not need to be declared in the code):
;
; 1) dirt_pct: this variable represents the percentage of dirty cells in the environment.
;
; 2) num_agents: number of vacuum cleaner agents in the simularion.
;
; 3) vision_radius: distance (in terms of number of cells) that the agents can 'see'
; For instance, if this radius is 3, then agents will be able to observe dirt in a cell that is 3 cells away from their own location.


; --- Global variables ---
; The following global variables are given.
;
; 1) total_dirty: this variable represents the amount of dirty cells in the environment.
; 2) time: the total simulation time.
; 3) num_ses: number of sensors around cleaners
globals [total_dirty time num_sens vacuum_ids undecided_color]


; --- Agents ---
; The following types of agent (called 'breeds' in NetLogo) are given.
;
; 1) vacuums: vacuum cleaner agents.
breed [vacuums vacuum]
breed [sensors sensor]


; --- Local variables ---
; The following local variables are given.
;
; 1) beliefs: the agent's belief base about locations that contain dirt
; 2) desire: the agent's current desire
; 3) intention: the agent's current intention
; 4) own_color: the agent's belief about its own target color
; 5) other_colors: the agent's belief about the target colors of other agents
; 6) outgoing_messages: list of messages sent by the agent to other agents
; 7) incoming_messages: list of messages received by the agent from other agents

vacuums-own [beliefs desire intention own_color other_colors outgoing_messages incoming_messages]


; --- Setup ---
to setup
  clear-all
  setup-variables
  setup-vacuums
  hide-all-links
  setup-patches
  setup-ticks
  draw-vision
end


to setup-variables
  set time 0
  set num_sens round(vision_radius * pi / 0.4) + 1
  set vacuum_ids []
  set undecided_color black
end


; --- Main processing cycle ---
to go
  ; This method executes the main processing cycle of an agent.
  ; For Assignment 4.1, this involves updating desires, beliefs and intentions, and executing actions (and advancing the tick counter).
  update-desires
  update-beliefs
  update-intentions
  execute-actions
  send-messages
  draw-vision
  tick
  set time ticks
  if finished? [ stop ]
end

to-report finished?
  report count vacuums with [desire = "stop"] = count vacuums
end

; --- Setup patches ---
to setup-patches
  ; set all clean
  ask patches [
    set pcolor white
  ]
  ; collect num_agents different colors for dirt
  let colors []
  let c_col 0
  let i 0
  while [ i < num_agents ] [
    set i i + 1
    ;-- checking if color is in the list already
    while [ member? c_col colors or c_col = 0 ] [
      set c_col 10 * (1 + random 13) + 5 ; only true colors, except 5, thus 15, 25,...,135
    ]
    set colors lput c_col colors
  ]
  ; scatter dirt randomly across dirt_pct of the board
  let width (max-pxcor - min-pxcor + 1)
  let height (max-pycor - min-pycor + 1)
  let ncells (width * height)
  let ndirt round(ncells * dirt_pct / 100)

  set total_dirty ndirt
  while [ ndirt > 0 ] [
    let x random-xcor
    let y random-ycor
    ; -- I check if it's unique turtle coordinate because otherwise same color turles blend with dirt
    while [ [pcolor] of patch x y != white ] [
;    while [ [pcolor] of patch x y != white or unique_turtle_coord? x y ] [
      set x random-xcor
      set y random-ycor
    ]
    ask patch x y [
      set pcolor one-of colors
    ]
    set ndirt (ndirt - 1)
  ]
end


; --- Setup vacuums ---
to setup-vacuums
  let i 0
  while [ i < num_agents ] [
    create-vacuums 1 [
      let x random-xcor
      let y random-ycor
      while [ unique_turtle_coord? x y ] [
        set x random-xcor
        set y random-ycor
      ]
      setxy x y
      set heading 0 ; 0 is North, 90 is East, ...
      set own_color undecided_color ;start with a neutral color - undecided_color
      set color own_color
      set incoming_messages []
      set outgoing_messages []
      set beliefs []
    ]
    let turtle_id (num_sens * i + i )
    create-sensors num_sens [
      set shape "dot"
      set color grey
      create-link-from turtle turtle_id
    ]
    set vacuum_ids lput turtle_id vacuum_ids
    set i i + 1
  ]
end

; -- checks if a turtle coordinate is unique
to-report unique_turtle_coord? [x y]
  report any? turtles with [ ycor = y and xcor = x ]
end

; --- Setup ticks ---
to setup-ticks
  reset-ticks
end

to hide-all-links
  ask links [hide-link]
end


; --- Update desires ---
to update-desires
  ; You should update your agent's desires here.
  ; Keep in mind that now you have more than one agent.
  ask vacuums [
    ifelse no-dirt-with-color? [
      set desire "stop"
    ] [
      set desire "clean"
    ]
  ]
end


to-report no-dirt-with-color?
  let c own_color ;necessary to avoid contex errors
  if own_color = undecided_color [ ; does not have a color yet
    report false
  ]
  report count patches with [ pcolor = c ] = 0
end

; --- Update beliefs ---
to update-beliefs
  ask vacuums [
    ifelse intention = "clear" and not empty? beliefs [
      set beliefs remove-item 0 beliefs
    ] [
      ifelse own_color = undecided_color [
        let other_dirts item 1 observe-dirt ; at this stage we only have other color dirt
        if not empty? other_dirts [
          set outgoing_messages remove-duplicate-messages (sentence outgoing_messages other_dirts)
        ]

        if choose-color? [
          let col own_color ; avoid context error
          ask out-link-neighbors [
            set color col
          ]
          set beliefs map [ item 0 ? ] filter [ item 1 ?1 = own_color ] outgoing_messages
          set outgoing_messages filter [ item 1 ?1 != own_color ] outgoing_messages
        ]
      ] [
        let dirts observe-dirt ; at this stage we will have several types of dirt (own and the one that we need to announce to other agents)
        let own_dirt item 0 dirts

        ; -- storing own dirt coordinates into beliefs
        if not empty? own_dirt [
          set beliefs sentence own_dirt beliefs
        ]

        ;-- storing new messages for other agents
        let other_dirt item 1 dirts
        if not empty? other_dirt [
          set outgoing_messages remove-duplicate-messages (sentence outgoing_messages other_dirt)
        ]

        ; -- storing dirt that has been communicated about by other agents
        let new_mes read-coords-messages
        set beliefs sentence new_mes beliefs
      ]
      ; -- final step
      set beliefs remove-duplicates beliefs
      set beliefs sort-by [dist ?1 < dist ?2] beliefs
    ]
  ]
end

to-report dist [ coordinate ]
  let x item 0 coordinate
  let y item 1 coordinate
  report (xcor - x) * (xcor - x) + (ycor - y) * (ycor - y)
end

to-report choose-color?
  let color_counts count-observed-dirt-colors
  set color_counts sort-by [ item 0 ?1 > item 0 ?2] color_counts
  let i 0
  let chose_color false
  while [ i < length color_counts and not chose_color ] [
    let col_count item i color_counts
    let c_count item 0 col_count
    let col item 1 col_count
    if c_count > beliefs_threshold and count vacuums with [ own_color = col ] = 0 [
      set own_color col
      set color own_color
      set chose_color true
    ]
    set i i + 1
  ]
  report chose_color
end

to-report count-observed-dirt-colors
  let color_counts []
  let i 0
  while [ i < length outgoing_messages ] [
    let col item 1 item i outgoing_messages
    let j 0
    let updated false
    while [ j < length color_counts and not updated ] [
      let color_count item j color_counts
      let new_count (1 + item 0 color_count)
      let new_color_count list new_count col
      set color_counts replace-item j color_counts new_color_count
      set updated true
      set j j + 1
    ]
    if not updated [
      set color_counts lput (list 1 col) color_counts
    ]
    set i i + 1
  ]
  report color_counts
end

; --- Update intentions ---
to update-intentions
  ask vacuums [
    if desire = "stop" [ set intention "no intention" ]
    if desire = "clean" [
      ; -- check if any of the items were observed
      ifelse empty? beliefs [
        set intention "search"
      ] [
        let dest item 0 beliefs
        ifelse dest = list xcor ycor [
          set intention "clear"
        ] [
          set intention "go to dirt"
        ]
      ]
    ]
  ]
end

;--- scan the area in the specified radius
;--- returns a list of found dirt of the passed color and other color dirts wrapped to messages
to-report observe-dirt
  let col own_color ; avoid context errors
  let own_dirt [] ; own color dirt
  let messages [] ; other agent dirt messages
  ask patches in-radius vision_radius [
    if pcolor = col [
      set own_dirt lput list pxcor pycor own_dirt
    ]
    if pcolor != col and pcolor != white [
      let mes (list (list pxcor pycor) pcolor false)
      if not (member? mes messages ) [
        set messages lput mes messages
      ]
    ]
  ]
  report (list own_dirt messages)
end

; --- Execute actions ---
to execute-actions
  ; Here you should put the code related to the actions performed by your agent: moving, cleaning, and (actively) looking around.
  ; Please note that your agents should perform only one action per tick!
  ask vacuums [
    if intention = "stop" [ stop ]
    if intention = "search" [ move ]
    if intention = "clear" [
      ask patch xcor ycor [
        set pcolor white
        set total_dirty total_dirty - 1
      ]
    ]
    if intention = "go to dirt" [
      let cleaning_task item 0 beliefs
      let x item 0 cleaning_task
      let y item 1 cleaning_task
      let action "move"
      if (towardsxy x y) != heading [ set action "rotate" ]
      if action = "rotate" [ facexy x y]
      if action = "move" [ ifelse distancexy x y > 1 [ fd 1 ] [setxy x y] ]
    ]
  ]
end

to draw-vision
  ask vacuums[
    let i 0
    let x xcor
    let y ycor

    ask out-link-neighbors [
      let angle (i * 2 * pi / num_sens) * 180 / pi
      set i i + 1
      let x_new (x + vision_radius * cos angle)
      let y_new (y + vision_radius * sin angle)
      ifelse x_new < max-pxcor and y_new < max-pycor and x_new > min-pxcor and y_new > min-pycor [
        show-turtle set xcor x_new set ycor y_new
      ] [
        hide-turtle
      ]
    ]
  ]
end


to move
  ifelse facing-wall? [
    rt 160
  ] [
    ifelse random-rotate? [
      set heading random 360
    ] [
      forward 1
    ]
  ]
end

to-report random-rotate?
  if random 100 < 20 [ report true ] ; random rotation
  report false
end


to-report facing-wall?
  ;show heading
  ; -- right wall
  if round(xcor) = max-pxcor and heading >= 0 and heading < 180 [report true]
  ; -- left wall
  if round(xcor) = min-pxcor and heading >= 180 and heading < 360 [report true]
  ; -- upper wall
  if round(ycor) = max-pycor and ( ( heading >= 270 and heading < 360 ) or (heading >= 0 and heading < 90 )) [report true]
  ; -- bottom wall
  if round(ycor) = min-pycor and (heading >= 90 and heading < 270 ) [report true]

  report false
end


; --- Send messages ---
to send-messages
  ask vacuums[
    if own_color = undecided_color [ ;do not send messages
      stop
    ]
    let i 0
    while [ i < (length outgoing_messages) ] [
      let mes item i outgoing_messages
      if not item 2 mes [ ;-- if it has not been sent
        send-message mes
        set mes replace-item 2 mes true
        set outgoing_messages replace-item i outgoing_messages mes
      ]
      set i i + 1
    ]
  ]
end

to send-message [mes]
  let col item 1 mes
  let coord item 0 mes
  ask vacuums with [own_color = col] [
    ; -- false stands for if a message has been read
    set incoming_messages remove-duplicate-messages (lput (list coord false) incoming_messages)
  ]
end


to-report read-coords-messages
  let i 0
  let new_cords []
  while [i < (length incoming_messages)] [
    let mes item i incoming_messages
    if not item 1 mes [ ;-- if it has not been read
      set new_cords lput (item 0 mes) new_cords
      set mes replace-item 1 mes true
      set incoming_messages replace-item i incoming_messages mes
    ]
    set i i + 1
  ]
  report new_cords
end


; -- removes duplicate messages based on coordinates
; -- under assumption that coords are at intdex 0
to-report remove-duplicate-messages [ coords ]
  let un_mes []
  let un_cords []
  let i 0
  while [ i < length coords ] [
    let mes item i coords
    let cord item 0 mes
    if not member? cord un_cords [
      set un_mes lput mes un_mes
      set un_cords lput cord un_cords
    ]
    set i i + 1
  ]
  report un_mes
end
@#$#@#$#@
GRAPHICS-WINDOW
819
10
1329
541
12
12
20.0
1
10
1
1
1
0
0
0
1
-12
12
-12
12
1
1
1
ticks
30.0

SLIDER
9
97
775
130
dirt_pct
dirt_pct
0
100
20
1
1
NIL
HORIZONTAL

BUTTON
9
65
393
98
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
392
65
775
98
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
8
249
774
282
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
9
134
775
167
num_agents
num_agents
1
7
3
1
1
NIL
HORIZONTAL

SLIDER
8
171
775
204
vision_radius
vision_radius
0
100
1
1
1
NIL
HORIZONTAL

MONITOR
446
280
773
325
Intention of vacuum 1
[intention] of vacuum 0
17
1
11

MONITOR
7
326
773
371
Desire of vacuum 1
[desire] of vacuum 0
17
1
11

MONITOR
210
281
446
326
Beliefs of vacuum 1
[beliefs] of vacuum 0
17
1
11

MONITOR
207
443
449
488
Beliefs of vacuum 2
[beliefs] of vacuum (item 1 vacuum_ids)
17
1
11

MONITOR
8
282
211
327
Color of vacuum 1
[own_color] of vacuum 0
17
1
11

MONITOR
9
443
208
488
Color of vacuum 2
[own_color] of vacuum (item 1 vacuum_ids)
17
1
11

MONITOR
449
443
772
488
Intention of vacuum 2
[intention] of vacuum (item 1 vacuum_ids)
17
1
11

MONITOR
9
487
772
532
Desire of vacuum 2
[desire] of vacuum (item 1 vacuum_ids)
17
1
11

MONITOR
10
605
207
650
Color of vacuum 3
[own_color] of vacuum (item 2 vacuum_ids)
17
1
11

MONITOR
206
605
449
650
Beliefs of vacuum 3
[beliefs] of vacuum (item 2 vacuum_ids)
17
1
11

MONITOR
448
605
774
650
Intention of vacuum 3
[intention] of vacuum (item 2 vacuum_ids)
17
1
11

MONITOR
10
650
774
695
Desire of vacuum 3
[desire] of vacuum (item 2 vacuum_ids)
17
1
11

MONITOR
9
18
775
63
Time to complete the task.
time
17
1
11

MONITOR
7
371
375
416
Outgoing messages of vacuum 1
[outgoing_messages] of vacuum 0
17
1
11

MONITOR
10
532
373
577
Outgoing messages of vacuum 2
[outgoing_messages] of vacuum (item 1 vacuum_ids)
17
1
11

MONITOR
10
694
374
739
Outgoing messages of vacuum 3
[outgoing_messages] of vacuum (item 2 vacuum_ids)
17
1
11

MONITOR
375
371
772
416
Incoming messages of vacuum 1
[incoming_messages] of vacuum (item 0 vacuum_ids)
17
1
11

MONITOR
372
532
772
577
Incoming messages of vacuum 2
[incoming_messages] of vacuum (item 1 vacuum_ids)
17
1
11

MONITOR
374
694
774
739
Incoming messages of vacuum 3
[incoming_messages] of vacuum (item 2 vacuum_ids)
17
1
11

SLIDER
8
210
774
243
beliefs_threshold
beliefs_threshold
0
20
5
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

ufo top
false
0
Circle -1 true false 15 15 270
Circle -16777216 false false 15 15 270
Circle -7500403 true true 75 75 150
Circle -16777216 false false 75 75 150
Circle -7500403 true true 60 60 30
Circle -7500403 true true 135 30 30
Circle -7500403 true true 210 60 30
Circle -7500403 true true 240 135 30
Circle -7500403 true true 210 210 30
Circle -7500403 true true 135 240 30
Circle -7500403 true true 60 210 30
Circle -7500403 true true 30 135 30
Circle -16777216 false false 30 135 30
Circle -16777216 false false 60 210 30
Circle -16777216 false false 135 240 30
Circle -16777216 false false 210 210 30
Circle -16777216 false false 240 135 30
Circle -16777216 false false 210 60 30
Circle -16777216 false false 135 30 30
Circle -16777216 false false 60 60 30

vacuum-cleaner
true
0
Polygon -2674135 true false 75 90 105 150 165 150 135 135 105 135 90 90 75 90
Circle -2674135 true false 105 135 30
Rectangle -2674135 true false 75 105 90 120

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

shape-sensor
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0

@#$#@#$#@
0
@#$#@#$#@
