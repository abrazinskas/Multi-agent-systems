; radius: the radius of the players circle
globals [rounds votes mafia_color citizen_color radius personalities opinions mafia_wins citizen_wins]

breed [players player]

; personality: corresponds to the personality type of player (naive, vengeful, logician)
; belief_social: how much player is influenced by believes of other players after the communication step
players-own [alive? role time belief_roles_mafia belief_roles_citizen belief_danger belief_social desire intentions action personality prev_vote]

to setup
  reset
  set mafia_wins 0
  set citizen_wins 0
end

to reset
  let mw mafia_wins
  let cw citizen_wins
  clear-all
  set mafia_wins mw
  set citizen_wins cw
  setup-ticks
  setup-variables
  setup-roles
  render-environment
end

to-report is-day?
  report ticks mod (length rounds) > 2
end

to setup-variables
  set rounds ["sleep" "m-vote" "kill" "awake" "discuss" "vote" "eliminate"]
  ask patches [set pcolor white]
  ; mafia and citizens will have their own color
  set mafia_color red
  set citizen_color blue
  set radius 5 ; can be changed to another value
  set personalities ["naive" "vengeful" "logician"]
  set opinions create-empty-list num-players -1
  reset-votes
end

to-report get-time
  report item (ticks mod (length rounds)) rounds
end

to reset-votes
  let num_players num-players
  set votes (list)
  let i 0
  while [i < num_players] [
    set votes lput 0 votes
    set i i + 1
  ]
end

to setup-roles
  let num_players num-players
  create-players num_players

  ; CHANGE THIS LIST TO ASSIGN PERSONALITY!
  let assigned_personality ["naive" "vengeful" "logician" "naive" "vengeful" "logician" "naive" "vengeful" "logician"]
  ; let assigned_personality ["logician""logician""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive""naive"]

  let i 0
  ask players [
    set belief_roles_mafia (create-empty-list num_players 0)
    set belief_roles_citizen (create-empty-list num_players 0)
    set belief_danger (create-empty-list num_players 0.5)
    set belief_social (create-empty-list num_players 0)
    set alive? true
    set shape "person"
    set label who
    set label-color black

    set time get-time
    set prev_vote -1
    setxy random-xcor random-ycor
    ; setting personality of a player at this stage randomly
    ifelse random_personality [
      set personality one-of personalities
    ] [
      set personality (item (who mod (length assigned_personality)) assigned_personality)
    ]
    ifelse who < num_mafia
    [
      set role "mafia"
      set color mafia_color
    ] [
      set role "citizen"
      set color citizen_color
    ]
    set i i + 1
  ]
  setup-citizen
  setup-mafia
end

to-report num-players
  report num_mafia + num_citizen
end

to setup-mafia
  let num_players num-players
  ; Setting belives of mafia
  ask players with [role = "mafia"][
      ;place holders to avoid variables overlap
      let brm belief_roles_mafia
      let brc belief_roles_citizen
      ; Mafia knows who are mafia and who are citizens
      ask players [
         ifelse role = "mafia"
         [set brm replace-item who brm 1]
         [set brc replace-item who brc 1]
      ]
      set belief_roles_mafia brm
      set belief_roles_citizen brc]
end

to setup-citizen
  ; Citizens initally only know about themself
  ; and they will think that other players
  ; can be equally mafia and citizans
  let believes (create-empty-list num-players 0.5)
  ask players with [ role = "citizen"][
    set belief_roles_citizen believes
    set belief_roles_mafia believes
    ; now we set the current player's belives about himself
    set belief_roles_citizen replace-item who belief_roles_citizen 1
    set belief_roles_mafia replace-item who belief_roles_mafia 0
  ]
end

to setup-ticks
  reset-ticks
end

to-report mafia-wins-fraction
  report ifelse-value (mafia_wins = 0) [ 0 ] [ mafia_wins / (mafia_wins + citizen_wins) ]
end


to go
  if finished? [
    let alive_mafia_count alive-mafia-count
    let alive_citizens_count alive-citizen-count

    if alive_mafia_count = 0 [
      set citizen_wins citizen_wins + 1
    ]

    if alive_citizens_count = 0 [
      set mafia_wins mafia_wins + 1
    ]

    reset
  ]
  ifelse is-day? [
    ask players [ set label-color black ]
  ] [
    ask players [ set label-color white ]
  ]
  update-time
  update-beliefs
  update-desires
  update-intentions
  execute-actions
  render-environment
  tick
end

to-report alive-mafia-count
  report count players with [role = "mafia" and alive?]
end

to-report alive-citizen-count
  report count players with [role = "citizen" and alive?]
end

to-report finished?
  let alive_mafia_count alive-mafia-count
  let alive_citizens_count alive-citizen-count
  report (alive_mafia_count = 0) or (alive_citizens_count = 0) ;  or (alive_mafia_count = alive_citizens_count)
end

; updates the time of the day and
; sets proper effects, e.g. bg color
to update-time
  ifelse is-day? [
    ask patches [set pcolor white]
  ] [
    ask patches [set pcolor black]
  ]
end

to update-beliefs
  ; Perceive from environment, which round it is and set it as a belief
  ask players [ set time get-time ]
  ; TODO: opinions should be somehow used here and then reset
  update-beliefs-mafia
  update-beliefs-citizen
  update-social-believes
  reset-opinions
end

to reset-opinions
  set opinions (create-empty-list num-players -1)
end
to update-beliefs-mafia
  ; Logically, beliefs should be updated at all times, but the function itself
  ; should actually introduce changes only when there is something to change
  ; i.e. There should be no need for this if statement
  ask players with [alive? and role = "mafia"] [
    if (time = "sleep") or (time = "awake") or (time = "vote") [
      ; TODO: Update danger for mafia
      let prev_votes ([prev_vote] of players)

      print (list "Player" who "sees previous votes:" prev_votes)

      let num_players num-players
      let i 0
      while [i < num_players] [
        ifelse not ([alive?] of player i) [
          ; if died then not dangerous
          set belief_danger (replace-item i belief_danger 0)
        ] [
          ; if previously voted against mafia
          ; UPDATE OF DANGER
          let player_prev_vote (item i prev_votes)
          if player_prev_vote != -1 [
            ; if a citizen has voted against *one of* mafia then we increase a danger
            let p (item i belief_danger)
            ifelse (([role] of player player_prev_vote) = "mafia") [
              ifelse (personality = "vengeful") [
                set belief_danger (replace-item i belief_danger (p + 0.2 * p * (1 - p)))
              ] [
                set belief_danger (replace-item i belief_danger (p + 0.1 * p * (1 - p)))
              ]
            ] [
              ifelse personality = "vengeful" [
                set belief_danger (replace-item i belief_danger (0.9 * p))
              ] [
                set belief_danger (replace-item i belief_danger (0.8 * p))
              ]
            ]
          ]
        ]
        set i i + 1
      ]
    ]
  ]
end


; this function updates believes of all players(both mafia and citizens)
; the logic is the following: it updates mafia social influence (social believes)
; based on only mafia opinions, and for citizens all opinions will be considered.
to update-social-believes
  if get-time != "vote" [stop] ; this belief has to be updated only after opinions exchange
  ask players with [alive?] [
    ; for now we simply reset social influece
    set belief_social create-empty-list num-players 0
    let i 0 ; starting position
    let finish num-players ; end position
    if role = "mafia"[
      set finish num_mafia
    ]
    while [i < finish] [
      let op item i opinions
      ; we assume that dead players have -1 opinions (because they are dead :( )
      ; we ignore the current players oppinions, as well as the ones who have no opinion,
      ; and if something thinks that the current is mafia
      if (i != who and op != -1 and op != who) [
        let num_against item op belief_social
        ; here comes personality factors
        let v 1 ; i.e. naive player believes everyone
        ; logician and vengeful do not trust everyone
        ; and accept votes proportionally to the danger belief
        if personality = "logician" or personality = "vengeful" [
          ; if danger is low the agent is willing to believe more
          ifelse not (bern-trial ( 1 - item i belief_danger)) [
            print (list "player" who "did not believe player" i "with danger" item i belief_danger)
            set v 0
          ][
            print (list "player" who "did believe player" i "with danger" item i belief_danger)
          ]
        ]
        set belief_social replace-item op belief_social (num_against + v )
      ]
      set i i + 1
    ]
    ; normalization because we want all elemnts to sum to 1
    set belief_social normalize-list belief_social
  ]
end

to update-beliefs-citizen
  ; Logically, beliefs should be updated at all times, but the function itself
  ; should actually introduce changes only when there is something to change
  ; i.e. There should be no need for this if statement
  ask players with [alive? and role = "citizen"] [
    if (time = "awake") or (time = "vote") [
      ; TODO: Update beliefs of citizens

      let prev_votes ([prev_vote] of players)
      ; anonimize roles, because citizens should not know who are citizens
      let roles ([ifelse-value alive? [-1] [[role] of player who]] of players)

      let num_players num-players
      let i 0
      while [i < num_players] [
        ifelse not ([alive?] of player i) [
          ; if died then not dangerous
          set belief_danger (replace-item i belief_danger 0)
        ] [
          ; if previously voted wrongly
          let player_prev_vote (item i prev_votes)
          if player_prev_vote != -1 [
            ifelse player_prev_vote = who [
              let p (item i belief_danger)
              ifelse (personality = "vengeful") [
                set belief_danger (replace-item i belief_danger (p + 0.2 * p * (1 - p)))
              ] [
                set belief_danger (replace-item i belief_danger (p + 0.1 * p * (1 - p)))
              ]
            ] [
              let p (item i belief_danger)
              ifelse personality = "vengeful" [
                set belief_danger (replace-item i belief_danger (0.9 * p))
              ] [
                set belief_danger (replace-item i belief_danger (0.8 * p))
              ]
            ]

            ifelse (item player_prev_vote roles = "citizen") [
              ; increase probability of mafia
              let p (item i belief_roles_mafia)
              ifelse personality = "vengeful" [
                set belief_roles_mafia (replace-item i belief_roles_mafia (p + 0.2 * p * (1 - p)))
              ] [
                set belief_roles_mafia (replace-item i belief_roles_mafia (p + 0.1 * p * (1 - p)))
              ]
            ] [
              let p (item i belief_roles_mafia)
              ifelse personality = "vengeful" [
                set belief_roles_mafia (replace-item i belief_roles_mafia (0.9 * p))
              ] [
                set belief_roles_mafia (replace-item i belief_roles_mafia (0.8 * p))
              ]
            ]
          ]
        ]
        set i i + 1
      ]
    ]
  ]
end

to update-desires
  update-desires-mafia
  update-desires-citizen
end

to update-desires-mafia
  ask players with [ alive? and role = "mafia" ] [
    ifelse is-day?
    [set desire  "hide" ]
    [set desire "kill citizens" ]
  ]
end

to update-desires-citizen
  ask players with [ alive? and role = "citizen" ] [
    ifelse is-day?
    [set desire  "find mafia" ]
    [set desire "sleep" ]
  ]
end

to update-intentions
  update-intentions-mafia
  update-intentions-citizen
end

to update-intentions-mafia
  ; time is a belief
  ; TODO: needs to be refined
  ask players with [alive? and role = "mafia"] [
    let intention ""
    if time = "sleep" [
      set intention "pretend to sleep"
    ]
    if time = "m-vote" [
      set intention "vote"
    ]
    if time = "kill" [
      set intention "kill"
    ]
    if time = "awake" [
      set intention "pretend to wake up"
    ]
    if time = "discuss" [
      set intention "exchange opinions"
    ]
    if time = "vote" [
      set intention "vote"
    ]
    if time = "eliminate" [
      set intention "eliminate"
    ]
    set intentions intention
  ]
end

to update-intentions-citizen
  ; time is a belief
  ; TODO: needs to be refined
  ask players with [alive? and role = "citizen"] [
    let intention "sleep"
    if time = "awake" [
      set intention "wake up"
    ]
    if time = "discuss" [
      set intention "exchange opinions"
    ]
    if time = "vote" [
      set intention "vote"
    ]
    if time = "eliminate" [
      set intention "eliminate"
    ]
    set intentions intention
  ]
end

to execute-actions
  let eliminate? false

  ask players with [alive?] [
    if (intentions = "sleep") or (intentions = "pretend to sleep") or
       (intentions = "wake up") or (intentions = "pretend to wake up") [
         set action intentions
    ]
    if (intentions = "exchange opinions") [
      exchange-opinions
      set action "exchange opinions"
    ]
    if (intentions = "vote") [
      vote
      set action intentions
    ]
    if (intentions = "kill") or (intentions = "eliminate") [
      set eliminate? true
      set action intentions
    ]
  ]

  if eliminate? [
    eliminate-player
    reset-votes
    ]

  ; execute-actions-mafia
  ; execute-actions-citizen
end

; set a vector of opinions
; -1 indicates that a player has not provided his opinion about who is mafia
to exchange-opinions
  ; Collect opinion of current player
  set opinions replace-item who opinions get-opinion
  print (list "player" who "thinks" (item who opinions) "is mafia")
end

; returns an id of a player who the current player suspect to be mafia
to-report get-opinion
  ifelse role = "mafia" [
    ;report (num_mafia + random num_citizen)
    report (get-max-index-random belief_danger)
  ] [
    report (get-max-index-random belief_roles_mafia)
  ]
end

; a general voting function that stores votes into a global variable
; the subsequent function has to eliminate a player who got most votes against
; works both for mafia (night) and all (day) voting
to vote
  let id get-vote ; the id of the player who player wants to eliminate
  print (list "player" who "wants to kill/eliminate" id)
  set votes replace-item id votes ((item id votes) + 1)
  set prev_vote id
end

; a function that outputs the id of a player who a player with [id] wants to eliminate most
; will involve a player type based heuristic
to-report get-vote
 let against -1 ; against whom a player wants to vote most
 let max_prob -1
 let weights (get-weights personality (role = "mafia")) ; a.k.a significance (lambdas)
 let i 0
 ; Argmax over all players
 while [i < num-players] [
   if ([alive?] of player i = true) and (role != "mafia" or [role] of player i != "mafia") [
     let mafia ((item i belief_roles_mafia) * (item 0 weights))
     let danger ((item i belief_danger) * (item 1 weights))
     let social ((item i belief_social) * (item 2 weights))
     ; print (list "player" who "prob" mafia danger social)
     let prob (mafia + danger + social)

     ; consider all factors and update the current target for elimination
     if prob > max_prob [
       set against i
       set max_prob prob
     ]
     ; this randomity for not to choose the first guy with the particular probability every time
     if (prob = max_prob) and (random 2 = 1) [
       set against i
     ]
   ]
   set i i + 1
 ]
 ; print (list "max prob" max_prob)
 report against
end

; the function eliminates the player who received majority of votes against
; TODO: if votes are equal then random selection is performed
to eliminate-player
;  let max_vote_player 0
;  let max_vote item 0 votes
;  let i 1
;  while [i < num-players] [
;    if max_vote < item i votes [
;      set max_vote (item i votes)
;      set max_vote_player i
;    ]
;    set i i + 1
;  ]
;
;  print max_vote
;  print max_vote_player

  ; returns an id of a player who got most votes against
  ; if tie - return random of maximums
  let max_vote_player get-max-index-random votes
  print (list "player" max_vote_player "is eliminated")
  ask player max_vote_player [
    set alive? false
    set desire 0
    set intentions 0
    set action 0
  ]
end

; returns a length of 3 weights list associated with a player type
; weights meaning:
;    1st: significance of mafia/citizen believes
;    2nd: significance of danger
;    3rd: significance of social influence
; note that weights for mafia are different
to-report get-weights [pers mafia?]
  let weights []

  ; if player is naive
  if pers = item 0 personalities
  [
    ifelse mafia?
    [set weights [0 0.5 0.5]]
    [set weights (create-empty-list 3 0.3333 )]
  ]

  ; if player is vengeful
  if pers = item 1 personalities
  [
    ifelse mafia?
    [set weights [0 0.7 0.3]]
    [set weights [0.2 0.6 0.2]]
  ]

  ; if logician (Tit for Tat)
  if pers = item 2 personalities
  [
    ifelse mafia?
    [set weights [0 0.65 0.35]]
    [set weights [0.5 0.4 0.1]]
  ]

  report weights
end

;to execute-actions-mafia
;  let num_players num-players
;  print "execute-action-mafia"
;  ask players with [role = "mafia" and alive? = true] [
;    ifelse is-day? [
;      ; Vote
;      let j 0
;      let voted false
;      while [j < num_players and not voted] [
;        if (j != who) and ([alive?] of player j = true) and ([role] of player j != "mafia") [
;          set votes replace-item j votes ((item j votes) + 1)
;          set voted true
;        ]
;        set j j + 1
;      ]
;    ] [
;      ; Kill citizens
;      let j 0
;      let voted false
;      while [j < num_players and not voted] [
;        if (j != who) and ([alive?] of player j = true) and ([role] of player j != "mafia") [
;          set votes replace-item j votes ((item j votes) + 1)
;          ; print j
;          set voted true
;        ]
;        set j j + 1
;      ]
;    ]
;  ]
;
;end
;
;to execute-actions-citizen
;  let num_players num-players
;  ask players with [role = "citizen" and alive? = true] [
;    if is-day? [
;      ; Vote
;      let j 0
;      let voted false
;      while [j < num_players and not voted] [
;        if (j != who) and ([alive?] of player j = true) [
;          set votes replace-item j votes ((item j votes) + 1)
;          set voted true
;        ]
;        set j j + 1
;      ]
;    ]
;  ]
;end

; updates the positions of players
; such that they are standing a circle
to render-environment
  let num_players num-players
  ask players with [not alive?] [
    set shape "x"
  ]
  ask players [
    let x 0 ; center of the canvas
    let y 0
    let angle (who * 2 * pi / num_players) * 180 / pi
    let x_new (x + radius * cos angle)
    let y_new (y + radius * sin angle)
    setxy x_new y_new
    facexy x y
  ]
end


;------------ SUPPORT FUNCTIONS -----------
; creates an empty list of length n with all values val
to-report create-empty-list [n val]
    let i 0
    let myList []
    while  [ i < n ][
      set myList lput val myList
      set i i + 1
    ]
    report myList
end

; returns the max item's index but if there are multiple with the same value
; the random will be returned
to-report get-max-index-random [l]
  let max_indices []
  let plh -99999999; placeholder
  let max_value get-max-value l
  let i 0
  ; collect indices with max value
  while [i < length(l)] [
    if (item i l) = max_value [
      set max_indices lput i max_indices
    ]
    set i i + 1
  ]
  report one-of max_indices ; return random
end

; returns the index of a maximum element from the list l
to-report get-max-index [l]
  let max_val -99999999
  let max_index -1
  let i 0
  while [i < length(l)] [
    if max_val < (item i l) [
      set max_index i
      set max_val (item i l)
    ]
    set i i + 1
  ]
  report max_index
end

to-report get-max-value [l]
  let max_id get-max-index l
  report item max_id l
end


; performs a Bernauli trial with
; success probabily p
to-report bern-trial [p]
  let r random-float 1.00001
  ifelse r < p[ report true] [report false]
end

; returns a normalized vector
to-report normalize-list [l]
   let i 0
   let s 0 ; sum
   ; finding normalization constant
   while [i < length(l)] [
     set s s + item i l
     set i i + 1
   ]
   set i 0
   ; normalizing
   let new_l []
   while [i < length(l)] [
     let val item i l
     if s != 0 [ set val val / s]
     set new_l lput val new_l
     set i i + 1
   ]
   ;print new_l
   report new_l
end
@#$#@#$#@
GRAPHICS-WINDOW
6
252
552
819
12
12
21.44
1
10
1
1
1
0
1
1
1
-12
12
-12
12
0
0
1
1
1.0

SLIDER
245
10
476
43
num_mafia
num_mafia
1
10
1
1
1
NIL
HORIZONTAL

BUTTON
8
10
82
43
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
84
10
150
43
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
150
10
246
43
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
475
10
692
43
num_citizen
num_citizen
1
30
19
1
1
NIL
HORIZONTAL

MONITOR
227
769
339
814
round
ifelse-value (ticks = 0) [\n  \"day\"\n] [\n  item ((ticks - 1) mod (length rounds)) rounds\n]
17
1
11

MONITOR
187
59
464
104
Believes about mafias
[belief_roles_mafia] of player 0
17
1
11

MONITOR
964
64
1045
109
Desire
[desire] of player 0
17
1
11

MONITOR
8
57
58
102
Role
[role] of player 0
17
1
11

MONITOR
467
59
714
104
Believes about danger
[belief_danger] of player 0
17
1
11

MONITOR
1046
64
1177
109
Intention
[intentions] of player 0
17
1
11

MONITOR
63
58
113
103
id
[who] of player 0
17
1
11

MONITOR
7
105
58
150
Role
[role] of player 1
17
1
11

MONITOR
62
106
114
151
id
[who] of player 1
17
1
11

MONITOR
186
107
462
152
Believes about mafia
[belief_roles_mafia] of player 1
17
1
11

MONITOR
466
107
713
152
Believes about danger
[belief_danger] of player 1
17
1
11

MONITOR
963
111
1046
156
Desire
[desire] of player 1
17
1
11

MONITOR
1047
111
1177
156
Intention
[intentions] of player 1
17
1
11

MONITOR
6
154
58
199
Role
[role] of player num_mafia
17
1
11

MONITOR
62
156
112
201
id
[who] of player num_mafia
17
1
11

MONITOR
186
155
464
200
Belives about mafia
[belief_roles_mafia] of player num_mafia
17
1
11

MONITOR
466
155
714
200
Believes about danger
[belief_danger] of player num_mafia
17
1
11

MONITOR
964
158
1045
203
Desire
[desire] of player num_mafia
17
1
11

MONITOR
1047
159
1177
204
Intention
[intentions] of player num_mafia
17
1
11

MONITOR
6
202
57
247
Role
[role] of player (num_mafia + 1)
17
1
11

MONITOR
63
202
113
247
id
[who] of player (num_mafia + 1)
17
1
11

MONITOR
187
202
464
247
Believes about mafia
[belief_roles_mafia] of player (num_mafia + 1)
17
1
11

MONITOR
467
203
712
248
Believes about danger
[belief_danger] of player (num_mafia + 1 )
17
1
11

MONITOR
965
207
1045
252
Desire
[desire] of player (num_mafia + 1)
17
1
11

MONITOR
1047
207
1175
252
Intention
[intentions] of player (num_mafia + 1)
17
1
11

MONITOR
177
278
393
323
number of votes against players
votes
17
1
11

MONITOR
115
59
185
104
Personality
[personality] of player 0
17
1
11

MONITOR
115
105
184
150
Personality
[personality] of player 1
17
1
11

MONITOR
115
155
185
200
Personality
[personality] of player num_mafia
17
1
11

MONITOR
117
200
186
245
Personality
[personality] of player (num_mafia + 1)
17
1
11

MONITOR
177
328
394
373
Opinions
opinions
17
1
11

MONITOR
1178
64
1307
109
Action
[action] of player 0
17
1
11

MONITOR
1177
111
1307
156
Action
[action] of player 1
17
1
11

MONITOR
1178
159
1307
204
Action
[action] of player num_mafia
17
1
11

MONITOR
1178
207
1307
252
Action
[action] of player (num_mafia + 1)
17
1
11

MONITOR
718
59
958
104
Social influence
[belief_social] of player 0
17
1
11

MONITOR
718
107
958
152
Social influence
[belief_social] of player 1
17
1
11

MONITOR
718
155
958
200
Social influence
[belief_social] of player num_mafia
17
1
11

MONITOR
719
203
959
248
Social influence
[belief_social] of player (num_mafia + 1 )
17
1
11

MONITOR
726
267
813
312
NIL
mafia_wins
17
1
11

MONITOR
617
267
707
312
NIL
citizen_wins
17
1
11

MONITOR
833
266
972
311
NIL
mafia-wins-fraction
2
1
11

SWITCH
708
10
906
43
random_personality
random_personality
0
1
-1000

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
