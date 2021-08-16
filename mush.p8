pico-8 cartridge // http://www.pico-8.com
version 29
__lua__
-- mushroom mtn.
-- by jsaba

function _init()
  printh(args",mush,1")
  cartdata"jsaba_mushroom_mountain"
  menuitem(5, "load game", loadgame)
  -- music(10, 0, 3)

  stepdur_org,stepdur,mountain_y,door_y,tmlx,tmrx,blinkon,screenshake,state,transition_state,endtext_y = args"6,6,46,60,4,38,1,0,title,in,666"

  playersprites,pal_mono,lastdowns,opdirs,bodylut,dissolve_particles,shovtarg,transition_clock,snakespr,gt,hidemain = tb"u=25;9,d=24;8,l=23;7,r=23;7",split"1,1,1,1,1,1,1,1,1,1,1,1,1,1,1",split"0,0,0,0,0,0,0",tb"u=d,d=u,l=r,r=l",tb"lr=37,rl=37,rr=37,ll=37,ud=36,du=36,uu=36,dd=36,ul=40,lu=40,ur=41,ru=41,dl=38,ld=38,dr=39,rd=39",{},vec(112,7),clock(.66),tb"u=32,r=33,d=34,l=35",clock(1)
  -- todo: can save 1 stupid token by inlining assignment w/ above
  fades_normal, fades_mono = fadetab"0,0,0,0,0,0_1,1,1,0,0,0_2,2,1,1,0,0_3,3,3,1,1,0_4,4,2,2,1,0_5,5,1,1,0,0_6,5,5,1,1,0_7,6,6,5,5,0_8,13,2,1,1,0_9,4,4,2,1,0_10,9,9,4,4,0_11,3,1,1,0,0_12,13,13,1,1,0_13,13,2,2,0,0_14,2,2,13,5,0_15,9,4,4,1,0", fadetab"0,0,0,0,0,0_1,1,1,0,0,0_1,1,1,1,0,0_1,1,1,1,1,0_1,1,1,1,1,0_1,1,1,1,0,0_1,1,1,1,1,0_1,1,1,1,1,0_1,1,1,1,1,0_1,1,1,1,1,0_1,1,1,1,1,0_1,1,1,1,0,0_1,1,1,1,1,0_1,1,1,1,0,0_1,1,1,1,1,0_1,1,1,1,1,0"

  setpath(10)
end

function fadetab(data)
  local result = {}
  for i, v in ipairs(split(data, "_")) do
    result[i-1] = split(v)
  end
  return result
end

function setpath(path, puz)
  --[[

    === blue path

    1. blue trailhead
    2. quiet walkway (single file)
    3. west rune site (intro rune door)
    4. damp grove (intro crate)
    5. switchback connector (crate stack)
    6. murmur cross (crate thru door + crate over water) 
    7. stillwood den (intro crawler)
    8. stillwood bluff (crawler over crate)
    9. stone garden (crate block crawler + crate thru door)
    10. hidden corner (intro poision)
    11. murmur bend (crate thru poison, crate thru and back door)
    * 12. crawler eat poison
    13. intro shovel

    n=,x=,y=,w=,h=,px=,py=
    n=bugs,x=0,y=0,w=8,h=6,px=0,py=0

    n=fog terrace,x=67,y=7,w=10,h=6,px=4,py=5 (crate thru poison, crate thru door, mush thru door)
    n=fog den,x=86,y=9,w=10,h=9,px=7,py=5 (advanced)
    n=night flower,x=15,y=8,w=7,h=3,px=0,py=1 (crate thru poison + dig behind)
    n=cave side,x=15,y=12,w=12,h=6,px=8,py=0 (advanced)
    n=evergreen vein,x=2,y=7,w=12,h=11,px=2,py=10 (advanced)
    n=zugg pocket,x=29,y=17,w=11,h=7,px=6,py=0 (advanced "spare the snake")
    n=sod plop,x=29,y=5,w=4,h=5,px=3,py=3 (crate thru poison + dig connect)
    n=minor garden,x=67,y=0,w=6,h=7,px=0,py=5 (dig hole to redirect, revolving door)
    n=gate,x=8,y=3,w=6,h=3,px=0,py=2 (intro dig a mushroom up? easy must dig poison)
    n=redirects,x=31,y=10,w=9,h=6,px=1,py=0,t=crawlers prefer_to avoid pits (advanced: 2 redirects and 2 rune doors)

    === red path

    1. red trailhead

    n=snakedoor,x=42,y=9,w=9,h=8,px=6,py=2 (moderate: easier version of make snakes crash)
    n=poison patch,x=43,y=18,w=9,h=7,px=6,py=5 (moderate)
    n=little bridge,x=23,y=8,w=5,h=3,px=0,py=2 (moderate)
    n=dig path,x=78,y=16,w=8,h=6,px=3,py=3 (moderate)
    n=snakes meet,x=54,y=18,w=10,h=8,px=4,py=3 (moderate: makes snake crash into each other)
    n=double trap,x=108,y=0,w=10,h=9,px=0,py=6 (advanced: 3 snakes)
    n=intro morph,x=49,y=10,w=4,h=7,px=0,py=6
    n=whisper hook,x=56,y=8,w=10,h=9,px=3,py=2 (advanced: morph, kill snake as snake, dig connector)
  ]]  
  
  puzzles, mapnodes, gpath, puzzledata = {}, {}, path, "n=blue trailhead,x=0,y=0,w=8,h=6,px=0,py=3,t=\020 blue trail \020 ;5 miles to peak;warning: this is;a perilous hike.;proceed at your;own risk.;ranger's tip:;plant mushrooms with x,blink_x=2,blink_y=3|n=quiet walkway,x=8,y=0,w=7,h=3,px=0,py=2|n=west rune site,x=15,y=0,w=6,h=5,px=0,py=4,t=the trails make use;of ancient rune doors;scattered throughout;the mountain.;please keep these;historical treasures of;mycological engineering;in good condition.;never attempt to;force a door open.|n=damp grove,x=31,y=1,w=6,h=4,px=0,py=1,t=use z to undo;|n=switchback connector,x=38,y=0,w=8,h=9,px=0,py=8|n=murmur brook,x=58,y=0,w=9,h=7,px=6,py=6|n=shade den,x=73,y=0,w=6,h=7,px=5,py=6|n=central cross,x=81,y=2,w=9,h=5,px=6,py=4|n=stone garden,x=91,y=0,w=8,h=6,px=5,py=5|n=west trap,x=100,y=6,w=7,h=2,px=6,py=1|n=murmur bend,x=47,y=1,w=7,h=6,px=6,py=5|n=felt copse,x=119,y=0,w=7,h=6,px=1,py=5,t=help maintain the trail.;dig up grass to make;new plots of dirt;by pressing x.;to keep the trail;beautiful for everyone;shovels will;dissolve after 1 use.|n=west sink,x=8,y=3,w=6,h=3,px=0,py=2|n=night flower,x=21,y=0,w=7,h=4,px=0,py=1|n=minor garden,x=67,y=0,w=5,h=7,px=0,py=5|n=fog terrace,x=67,y=7,w=10,h=6,px=4,py=5|n=ink pond,x=86,y=7,w=10,h=9,px=7,py=5|n=sod plop,x=29,y=5,w=4,h=5,px=3,py=3|n=zugg pocket,x=29,y=17,w=11,h=7,px=6,py=0|n=cave side,x=15,y=12,w=12,h=6,px=8,py=0|n=evergreen vein,x=2,y=7,w=12,h=11,px=2,py=10"

  if (gpath == 11) puzzledata = "n=red trailhead,x=15,y=7,w=6,h=4,px=5,py=2,t=\020 red trail \020 ;3 miles to peak;notice: this trail;is recommended for;expert hikers only.;the trail is short but;steep. cultivate red;mushrooms to proceed.|n=slip bog,x=23,y=8,w=5,h=3,px=0,py=2|n=shade garden,x=42,y=9,w=7,h=8,px=6,py=2|n=east rune site,x=43,y=18,w=9,h=7,px=6,py=5|n=crooked pith,x=78,y=9,w=8,h=6,px=3,py=3|n=mute creek,x=54,y=18,w=10,h=8,px=4,py=3|n=slink den,x=108,y=0,w=10,h=9,px=0,py=6|n=ancient stone,x=49,y=10,w=6,h=6,px=0,py=5|n=whisper hook,x=56,y=8,w=11,h=9,px=3,py=2"

  -- puzzledata = "n=intro morph,x=49,y=10,w=6,h=6,px=0,py=5"

  -- tokens: can combine with statement below? careful w/ order of operations for asignments (if last assigned first can't do it)
  peakindex = #split(puzzledata, "|")

  puzzledata = split(puzzledata.."|n=cabin at west peak,x=103,y=21,w=9,h=10,px=4,py=8,t=\022 ;west overlook;\023 ;mush. mt. volunteer lodge,191=4,174=14|n=cabin from landing,x=103,y=21,w=9,h=9,px=5,py=6,t=\022 ;west overlook;\023 ;mush. mt. volunteer lodge,191=4,174=14|n=cabin from back path,x=103,y=21,w=9,h=9,px=2,py=0,t=\022 ;west overlook;\023 ;mush. mt. volunteer lodge,174=14,191=4|n=landing from cabin,x=97,y=10,w=4,h=4,px=1,py=3,157=2,158=7,190=9|n=landing from bed,x=97,y=10,w=4,h=4,px=0,py=1,157=2,158=7,190=9|n=landing from closet,x=97,y=10,w=4,h=4,px=3,py=1,158=7,190=9,157=2|n=bed from landing,x=103,y=9,w=5,h=4,px=0,py=1,190=11,159=5|n=bed from deck,x=103,y=9,w=5,h=4,px=1,py=0,190=11,159=5|n=closet from landing,x=97,y=14,w=4,h=2,px=3,py=1,157=6,159=16|n=closet from basement,x=97,y=14,w=4,h=2,px=0,py=0,159=16,157=6|n=deck from bed,x=113,y=19,w=8,h=8,px=2,py=7,157=8,174=22,156=15|n=deck from back path,x=113,y=19,w=8,h=8,px=0,py=0,157=8,156=15,174=22|n=deck from star bridge,x=113,y=19,w=8,h=8,px=3,py=0,174=22,156=15,157=8|n=back path from cabin,x=123,y=7,w=5,h=5,px=1,py=4,156=3,174=12|n=back path from deck,x=123,y=7,w=5,h=5,px=3,py=0,174=12,156=3|n=basement from closet,x=109,y=14,w=5,h=4,px=0,py=0,158=10,174=18|n=basement from mauso,x=109,y=14,w=5,h=4,px=0,py=0,158=10,174=18|n=mauso from basement,x=115,y=10,w=7,h=2,px=0,py=0,174=17,157=21|n=mauso from cemetery,x=115,y=10,w=7,h=2,px=6,py=1,174=17,157=21|n=cemetery at east peak,x=67,y=15,w=15,h=11,px=13,py=10,t=\023;east overlook;\024;mausoleum,156=20,183=19,174=20|n=cemetery from mauso,x=67,y=15,w=15,h=11,px=13,py=10,t=\023;east overlook;\024;mausoleum,183=19,174=20,156=20|n=star bridge from deck,x=125,y=19,w=3,h=9,px=1,py=8,156=13,174=24|n=star bridge from juncture,x=125,y=19,w=3,h=9,px=2,py=0,174=24,156=13|n=juncture from star bridge,x=125,y=28,w=3,h=3,px=1,py=2,t=\022 ;west overlook;\023 ;hot springs,157=27,174=30,156=23|n=juncture from west overlook,x=125,y=28,w=3,h=3,px=0,py=1,t=\022 ;west overlook;\023 ;hot springs,174=30,157=27,156=23|n=juncture from cave,x=125,y=28,w=3,h=3,px=2,py=1,t=\022 ;west overlook;\023 ;hot springs,157=27,174=30,156=23|n=cave from juncture,x=121,y=32,w=7,h=4,px=0,py=1,174=26,183=29|n=cave from spring,x=121,y=32,w=7,h=4,px=4,py=1,174=26,183=29|n=spring from cave,x=123,y=28,w=2,h=3,px=1,py=2,156=28|n=west overlook from juncture,x=88,y=16,w=14,h=14,px=13,py=7,174=25", "|")

  -- build puzzles data
  for data in all(puzzledata) do
    add(puzzles, tb(data))
  end

  -- build progress map nodes
  for x=42,52 do
    for y=26,35 do
      local s,si = mget(x, y) 
      if (path == 11 and s > 31) si = s - 31
      if (path == 10 and s > 0 and s < 32) si = s
      if (si) add(mapnodes, {p=vec((x-40)*8, (y-27)*8+12), i=si})
    end
  end

  setpuzzle(puz or 1)
end

function tb(data, da, db, usenum)  
  local r = {} 
  for s in all(split(data, da or ",")) do
    local k, v = args(s, db or "=")
    local vtab = split(v, ";")
    r[usenum and k or tostr(k)] = (#vtab > 1) and vtab or v
  end
  return r
end

function setpuzzle(i)
  pz, pz_i = puzzles[i], i
end

function init_nextpuzzle()
  if thruitem then
    state,transition_state,transition_animate,hidemain = "heading","in"
    if (thruitem.topuz) setpuzzle(thruitem.topuz)
    init_puzzle()
  else
    setpuzzle(pz_i + 1)
    init_heading()
  end
end

function loadgame()

  if dget(60) then
    setpath(dget(61), dget(60))
    highest, solved, mountain_y, transition_animate = dget(62), {}, 40
    for i=0,59 do
      if dget(i) and dget(i) == 1 then 
        add(solved, i)
      else
        dset(i, 0)
      end
    end
    init_heading()
  end
end

function savegame()
  dset(60, pz_i)
  dset(61, gpath)
  dset(62, highest)
  for i=0,59 do
    dset(i, contains(solved, i) and 1 or 0)
  end
end

function init_heading()
  playsfx"-2"
  -- clear reset, to map, skip puzzle
  foreach(split"1,2,3", menuitem)
  menuitem(4, "save game", savegame)
  state,transition_state,transition_animate,hidemain = "heading","in",transition_animate and update_heading_transition
  if (pz_i > peakindex) setpuzzle(peakindex + 1)
  if (pz_i > highest) highest = pz_i
end

function init_puzzle()
  menuitem(1, "restart puzzle", loadmap)
  menuitem(2, "to map", init_heading)
  if (pz_i <= peakindex) menuitem(3, "skip puzzle", skip)
  loadmap()
end

function skip()
  -- add(solved, pz_i)
  setpath(gpath, pz_i + 1)
  init_heading()
end

function sparkle(i, data)
  if (gt.t % (i or 6) == 0) dissolve_cluster(tb"x=5,y=5,w=110,h=110", data)
end

function _update()
  if (gt.t % 6 == 0) blinkon = not blinkon 
  if (transition_state) update_transition()
  local go = btnp(4) or btnp(5) and not transition_state
  if state == "puzzle" then
    if (starview) sparkle(12, "12,7,8,4") -- col,part,rad,dur
    if windelay then 
      clocktick(windelay)
      if (windelay.perc == 1) transition_next,transition_state,forcefade,hidemain,windelay = init_nextpuzzle,args"out,1,1"
    end
    -- update puzzle
    if signtext then 
      sign_t = min(sign_t + (sign_hiding and -.12 or .12), 1)
      if sign_t == 1 and btnd() then 
        pl.pos = pl_lastpos
        if sign_i+1 < #sign_texts then
          sign_i += 2
          signtext,signtext_2 = sign_texts[sign_i],sign_texts[sign_i+1]
          signto = signtarget(signtext)
        else
          sign_hiding = true
        end
        playsfx"7"
      end
      signpos = lerpvec(signpost_pos, signto, sign_t, smooth)
      if sign_hiding then
        if (sign_t <= 0) signtext, sign_hiding = nil
      else
        local x, y = tb"r=3,l=-3,u=0,d=0"[nextmovedir], tb"r=0,l=0,u=-3,d=3"[nextmovedir]
        pl.pos, sign.pos = lerpvec(pl_lastpos, pl_lastpos + vec(x, y), sin(-min(sign_t*2,1)/2)), lerpvec(signpost_pos, signpost_pos + vec(x, y), sin(max(sign_t*2,1)/2))
      end
    elseif not endgame then
      if not won then
        update_pz_input()

        --update puzzle commands
        if steptime > 0 then
          if steptime == stepdur then 
            -- if (undo) playsfx"0,2"
            placehud_pos = nil
          end
          local begin = steptime == stepdur
          steptime -= 1
          stepperc = (stepdur - steptime) / stepdur

          local stack = pl.commands
          if (begin) stack[#stack].start()
          if #stack > 0 then
            local c = stack[#stack]
            c.run(stepperc)
            if stepperc == 1 then
              c.next()
              if (undo) deli(stack)
            end
          end

          -- finish step
          if steptime == 0 then
            refresh_hud()
            undo = false
          end
        end
      end
      if (placehud_pos) clocktick(placehud_clock)

      -- update wonspark
      if wonspark_pos then
        clocktick(wonspark_clock)
        if (wonspark_clock.perc == 1) wonspark_pos = nil
      end
    else -- endgame
      endgame += 1
      if endgame == 30 then
        -- todo: don't forget to clear endgame at some point
        state, endgame, sleepystart = "endscreen", 0, sleepyhead
      end
    end
  end

  if containsdata("heading,title", state) then
    sparkle(12, "12,3,10,4")
    if go then
      transition_state = "out"
      if state == "heading" then 
        playsfx"2"
        if (gpath == 11 and pz_i > peakindex) setpuzzle(peakindex + 21)
        transition_next, transition_animate = init_puzzle
      else
        -- playsfx"16" -- todo: only playing one sfx now?
        playsfx"17"
        highest, transition_next, transition_animate, solved = 0, init_heading, update_title_transition, {}
      end
    end
    if not transition_state and state == "heading" then
      if highest == 1 then
        for key in all(split"2,3") do
          if (btnp(key)) setpath(gpath == 10 and 11 or 10)
        end
      else
        if btnp(2) and pz_i < highest then
          setpuzzle(pz_i + 1)
        elseif btnp(3) then
          setpuzzle(max(pz_i - 1, 1))
        end
      end
    end
  elseif state == "endscreen" then
    local endoffset, kf1 = 30, 90
    endgame += 1
    if endgame > endoffset and endgame <= kf1 then
      local endtime =  (endgame - endoffset) / (kf1 - endoffset)
      sleepyhead, endtext_y = lerpvec(sleepystart, vec(56, 70), endtime, sineout), lerp(-6, 15, endtime, sineout)
    elseif endgame > kf1 then
      sparkle(12, "12,7,8,4") -- col,part,rad,dur
    end
  end

  -- update dissolve
  for p in all(dissolve_particles) do
    clocktick(p.clock)
    if (p.clock.perc == 1) del(dissolve_particles, p)
  end

  for i=0,5 do
    lastdowns[i] = btn(i)
  end
  lastdowns[6] = btn()
  clocktick(gt)
end

function update_title_transition()
  door_y,mountain_y,tmlx,tmrx = lerp_trans"60,94",lerp_trans"46,43",lerp_trans"4,-44",lerp_trans"38,86"
end

function update_heading_transition()
  door_y,mountain_y,tmlx,tmrx = lerp_trans"94,128,1",lerp_trans"43,40,1",lerp_trans"-44,-92,1",lerp_trans"86,134,1"
end

function lerp_trans(data) 
  local d = split(data)
  return lerp(d[1], d[2], transition_clock.perc, d[3] and sineout or sinein)
end

function dissolve_cluster(pos_or_rect, data)
  local col, parts, rad, dur = args(data)
  rad = tonum(rad)
  for i=1,parts do
    local p
    if pos_or_rect.w then 
      p = vec(pos_or_rect.x + rnd(pos_or_rect.w), pos_or_rect.y + rnd(pos_or_rect.h))
    elseif rad then 
      p = pos_or_rect + pvec(rnd(8)-4, rnd(1)) + vec(4,4)
    end
    add(dissolve_particles, {
      col=col,
      startpos=p, 
      endpos=p + (rad and pvec(rnd(4) + rad, rnd(1)) or vec(0,0)),
      clock=clock(rnd(.4) + dur)
    })
  end
end

function player_dissolve_command(cell, dur, doflash, fromsnake)
  local c = fade_command(pl)
  local s = c.start
  c.start = function()
    s()
    if (undo) return
    playsfx(fromsnake and "9" or "19")
    for col in all(split"7,13,2") do
      dissolve_cluster(cellpos(cell), col..",8,4,"..dur)
    end
    if (doflash) sfl_c,sfl_t = 8,4
  end
  return c
end

function splash_command(item, cell)
  local c = fade_command(item)
  local s = c.start
  c.start = function()
    s()
    if not undo then
      playsfx(item == pl and "15" or "14")
      dissolve_cluster(cellpos(cell), "12,12,1,.3")
    end
  end
  return c
end

function canpushto(to, dir, thrudoor)
  if (blocktab.item(to)) return canpushto(stepcell(to, dir), dir, thrudoor)

  local seg = is_snake(to)
  local snake = seg and seg.snake

  if #items(to) > 0 and 
    not baseobstacletab.item(to) and
    (not seg or snake and snake.body[#snake.body] == seg and not is_snake(to, true)) and 
    not shoveltab.item(to) then

    local indoor, outdoor = doorpair(to)
    if outdoor then
      if outdoor.open and not thrudoor then
        return canpushto(stepcell(outdoor.cell, "d"), "d", true)
      else
        return
      end
    elseif indoor == goaldoor then
      return
    end
    return to, thrudoor
  end
end

function update_pz_input()
  if (not btn(2) and (lastdowns[2] == true)) invertcontrols = false
  if steptime == 0 then
    undo = (btn(4) and #pl.commands > 0)
    stepdur = undo and flr(stepdur_org*.7) or stepdur_org
    local nextcell

    if not undo then
      local pl_cell = pl.cell
      local maybenext = stepcell(pl_cell, btn(0) and "l" or btn(1) and "r" or btn(2) and (invertcontrols and "d" or "u") or btn(3) and "d")
      local _, outdoor = doorpair(maybenext)
      local nextdoor = doortab.item(maybenext)
      local dooropen = nextdoor and nextdoor.open
      nextmovedir = dircell(pl_cell, maybenext or pl_cell)
      nextcell = (dooropen and outdoor) and stepcell(outdoor.cell, "d") or maybenext
      local facecell = stepcell(pl_cell, pl_dir)
      local nextmoved, nextmush, nextshovel =  pl_cell ~= nextcell, mushtab.item(nextcell), shoveltab.item(nextcell)

      -- handle snake control modes
      if snakemode then
        if nextmoved and not is_snake_obstacle(pl_snake, stepcell(pl_snake.cell, nextmovedir)) then 
          steptime, userdir = stepdur, nextmovedir
        elseif btn(5) then -- todo: tokens (can remove if needed?: nice detail but not essential dept on peak area secrets)
          steptime, userdir = stepdur
        end
      end

      if nextmoved and signtab.item(nextcell) then
        signto, signtext, signtext_2, sign_t, sign_i = signtarget(sign_texts[1]), sign_texts[1], sign_texts[2], 0, 1
        pl_lastpos = pl.pos
        playsfx"11"
        return
      end

      -- push block
      local pushblock,pushto,pushthru = blocktab.item(nextcell)
      if pushblock then
        pushto = stepcell(pushblock.cell, dooropen and opdirs[nextmovedir] or nextmovedir) 
        local pushpath,d = true,doortab.item(pushto) 
        if (d) pushpath = d.open and nextmovedir == "u"
        if pushpath then
          pushto, pushthru = canpushto(pushto, (d or dooropen) and "d" or nextmovedir, dooropen)
        else 
          pushto = nil
        end
        willblock = pushto
      end

      mainbatch = {}

      -- player moved and is in bounds and not blocked
      if not transition_state and
        not died and 
        #items(nextcell) > 0 and 
        not baseobstacletab.item(nextcell) and
        nextmoved and 
        ((not nextdoor) or (dooropen and nextmovedir == "u")) and 
        ((not pushblock) or pushto or pushblock.inwater) then 
        -- trigger step:

        steptime, willmoveto = stepdur, nextcell
        local move, enterdoor = moveplayer_command(pl_cell, nextcell), nextdoor and nextdoor.open or false
        addbatch(move)

        thruitem = thrutab.item(nextcell)
        if thruitem then
          fadepl()
          addbatch(win_command())
        end

        if enterdoor then
          if nextdoor.isgoal then 
            fadepl()
            addbatch(win_command(nextdoor))
          else
            mainbatch = {thrudoor_command(maybenext)} 
          end
        end

        if pushto then 
          -- add push block commands
          local movedir = pushthru and "d" or nextmovedir
          local curcell = stepcell(pushto, opdirs[movedir])
          local _, od = doorpair(curcell)
          if od then
            curcell = stepcell(od.cell, "d")
            movedir = opdirs[movedir]
          end

          while blocktab.item(curcell) do
            local pushcell = stepcell(curcell, movedir)
            addbatch(movepushblock_command(blocktab.item(curcell), pushcell))
            curcell = stepcell(curcell, opdirs[movedir])

            local _, od = doorpair(curcell)
            if od then
              curcell = stepcell(od.cell, "d")
              movedir = "u"
            end
          end
        end
        
        if nextmush then
          steptime = stepdur
          if nextmush.type == 26 then
            addbatch(delmush_command(nextmush, true)) -- pick
          else
            addbatch(player_dissolve_command(nextcell, 1.5))
            addbatch(die_command())
            addbatch(delmush_command(nextmush))
          end
        elseif watertab.item(nextcell) and not waterblock(nextcell) then
          addbatch(splash_command(pl, nextcell))
          addbatch(die_command())
        end
        if (nextshovel) addbatch(getshovel_command(nextshovel))
      end

      -- actions
      if stepdur ~= steptime and not died and btnd(5) then
        local attackseg = canattack(facecell)
        
        local actionfx = can_place_mush(facecell) and addmush_command(facecell, 26, "4") or (digtype(facecell) and dig_command(facecell)) or (attackseg and killsnake_command(attackseg.snake, true))

        if actionfx then
          steptime = stepdur
          addbatch(actionfx)
        end
      end

      if (died and stepdur ~= steptime and (btnd(5) or nextmoved)) steptime = stepdur

      -- predict snake
      local seg = is_snake(nextcell or pl_cell, true)  
      if seg and not seg.snake.willdie and not died then
        addbatch(player_dissolve_command(nextcell, .6, true, true))
        addbatch(die_command())
      end

      -- todo: change this to just be if any commands?
      if steptime == stepdur then  
        add_snake_commands()
        add_grow_commands(mushpairs())
        add_grow_commands(mushpairs(filterpoison))
        check_doors()
        add(pl.commands, batch_command(mainbatch, true))
        willadd_shrooms, willdel_shrooms,changedirt,willblock,willmoveto = {},{},0
      end
    else
      steptime = stepdur    
    end
  end
end

function addbatch(c)
  add(mainbatch, c)
end

function fadepl()
  pl_mask = false
  addbatch(fade_command(pl))
end

function mushpairs(filter)
  return filterpairs(mushtab.items, filter or filtermush)
end

function filtermush(k, v)
  return v.type == 26
end

function filterpoison(k, v)
  return v.type == 12
end

function check_doors(startup)
  local dct = 0
  for _, dirt in pairs(dirttab.items) do
    dct += 1
  end

  for _, door in pairs(doortab.items) do
    local change, full, hc = 0
    if door.isgoal then
      if (door.flag == 19) full = #mushpairs() + #filterpairs(willadd_shrooms, filtermush) - #filterpairs(willdel_shrooms, filtermush) == dct + changedirt
      if (door.flag == 3) full = #mushpairs(filterpoison) + #filterpairs(willadd_shrooms, filterpoison) - #filterpairs(willdel_shrooms, filterpoison) == dct + changedirt
      if (dct == 1 and changedirt == -1) full = false
      if (dct == 0) full = false
    else
      local flag = door.flag
      for _, m in pairs(willadd_shrooms) do
        if (m.flag == flag) change += 1
      end
      for _, m in pairs(willdel_shrooms) do
        if (m.flag == flag) change -= 1
      end
      local runect = dirttab.itemcount(flag)
      full = (mushtab.itemcount(flag) + change == runect) and runect > 0
    end

    if full then
      if not door.open then 
        addbatch(setdoor_command(door))
        hc = true
      end
    elseif door.open then -- close
      local _, other = doorpair(door.cell)
      local s1,s2 = is_snake(stepcell(door.cell, "d"), true),other and is_snake(stepcell(other.cell, "d"), true)
      if not s1 or not s2 or s1.snake ~= s2.snake then
        addbatch(setdoor_command(door, true))
        hc = true
      end
    end
    if (not hc) add(door.commands, command()) -- empty
    if (startup and full) door.open, door.spr = true, door.isgoal and (gpath == 10 and 17 or 181) or 18
  end
end

function killsnake_command(snake, byshovel, excludecel)
  snake.willdie = true
  local c = fade_command(snake)
  local s,n,converted,killcels = c.start,c.next,{},{snake.cell}

  -- setup poison mush + dirt 
  for b in all(snake.body) do
    add(killcels, b.cell)
    addbatch(fade_command(b))
  end
  if not byshovel then
    killcels[#killcels] = nil
    if (not doorpair(stepsnake(snake))) add(killcels, stepsnake(snake))
  end
  for cel in all(killcels) do
    if not willadd_shrooms[vecstring(cel)] and not waterblock(cel) then
      if not dirttab.item(cel) then
        grasstab.del(cel)
        createdirt(cel, 1)
        add(converted, cel)
      end
      if (cel ~= excludecel) addbatch(addmush_command(cel, 12))
    end
  end

  c.start = function()
    snake.willdie = false
    s()

    if undo then 
      for cel in all(converted) do
        dirttab.del(cel)
        creategrass(cel)
      end

      snaketab.add(snake)
      if (byshovel) shovels += 1
    else 
      -- dissolve particles
      dissolve_cluster(cellpos(snake.cell), "7,5,2,1.5")
      for b in all(snake.body) do
        dissolve_cluster(b.pos, "7,5,2,1.5")
      end  

      if byshovel then
        shovels -= 1
        sfl_c,sfl_t = 1,4
        playsfx"12"
      else
        playsfx"21" -- 19 was poison
      end
    end
  end
  c.next = function()
    n()
    if (not undo) snaketab.del(snake.cell)
  end
  if (snake.user) addbatch(die_command())
  return c
end

function dig_command(cell)
  local c,dt,mu,di= command(),digtype(cell),mushtab.item(cell),dirttab.item(cell)
  if (dt == 22) willdel_shrooms[vecstring(mu.cell)] = mu
  willdig_at,changedirt = cell,dt == 1 and 1 or -1

  c.start = function()
    willdig_at = nil
    if undo then
      shovels += 1
      if dt == 1 then
        dirttab.del(cell)
        creategrass(cell)
      else
        holetab.del(cell)
        mushtab.add(mu)
        dirttab.add(di)
      end
    else
      playsfx"10"
      shovels -= 1
      if dt == 1 then
        grasstab.del(cell)
        createdirt(cell, 1)
      else
        mushtab.del(cell)
        dirttab.del(cell)
        createitem(cell, 22, nil, holetab)
      end
      dissolve_cluster(cellpos(cell), "9,6,2.4,.25")
    end
  end
  return c
end

function can_place_mush(cell)
  return dirttab.item(cell) and #items(cell) == 1 and not is_snake(cell)
end

function refresh_hud()
  placehud_pos = nil
  if (died) return
  local facecell = stepcell(pl.cell, pl_dir)
  if (can_place_mush(facecell)) new_placehud(facecell, 10)
  if (digtype(facecell) == 1 or digtype(facecell) == 22) new_placehud(facecell, 6)
  if (canattack(facecell)) new_placehud(canattack(facecell).cell, 6)
end

function new_placehud(cell, sprite)
  placehud_pos, placehud_spr, placehud_clock = cellpos(cell), sprite, clock(1.79)
end

function digtype(cell)
  if shovels > 0 and 
  not is_snake(cell) and
  (grasstab.item(cell) and #items(cell) == 1 or mushtab.item(cell)) then
    if (mushtab.item(cell)) return 22
    if (grasstab.item(cell)) return 1
  end
end

function canattack(cell)
  if (shovels > 0) return is_snake(cell)
end

-- draws

function sprtb(d)
  local a = split(d)
  spr(a[1],a[2],a[3],a[4] or 1,a[5] or 1,a[6]==1,a[7]==1)
end

function draw_dissolve(ramp)
  for p in all(dissolve_particles) do
    local t = p.clock.perc
    local pos = lerpvec(p.startpos, p.endpos, t, sineout)
    pset(pos.x, pos.y, lerptab((ramp or fades())[p.col], t))
  end
end

function clearpal() 
  pal(died and pal_mono or nil)
end

function _draw()
  cls()
  clip(args"4,4,120,120")

  if transition_state then 
    draw_fade(transition_clock.perc, transition_state)
    if (transition_animate) transition_animate()
  end
  if state == "puzzle" then
    if not windelay and not hidemain then
      if (starview) draw_dissolve()
      if not transition_state then
        clearpal()
      end
      foreach({grasstab, dirttab, watertab, waterblocktab, holetab, baseobstacletab, signtab, thrutab, morphtab}, draw_items_ct)
      draw_items(mushtab.items, nil, mushspr)
      draw_items_ct(blocktab)
      -- snakes
      for _, s in pairs(snaketab.items) do
        if (s.user) pal(8,13)
        foreach(s.body, draw_obj)
        draw_obj(s)
      end
      draw_items(doortab.items, isnotgoal)
      draw_item(outblock)
      foreach(outsnakes, draw_obj)
      draw_items_ct(shoveltab)
      -- hud
      if placehud_pos and placehud_clock.perc and ceil(10*placehud_clock.perc)%2 == 0 then
        celmask(placehud_pos)
        pal(pal_mono)
        spr(placehud_spr, placehud_pos.x, placehud_pos.y)
        clearpal()
      end
      if shovels > 0 then
        for i=1, shovels do
          spr(6,120-i*8,7)
        end
      end

      if (not blunk and pz.blink_x) blinkpos = cellpos(vec(pz.blink_x, pz.blink_y))
      if blinkpos and not blunk and blinkon then 
        palt(0b0000000000000000)
        spr(182, blinkpos.x, blinkpos.y)
        palt()
      end

    end
    -- above transition
    if (not forcefade) pal()
    if (goaldoor) draw_obj(goaldoor)
    if signtext and signpos then 
      blunk = true
      local sres = lerpvec(signpost_pos, vec(8, 7), sign_t, smooth)
      clip(sres.x, sres.y, max(112*sign_t,6), max(32*sign_t,3))
      rectfill(args"0,0,128,128,0")
      map(56,30,sres.x,sres.y,14,4)
      print(signtext, signpos.x, signpos.y, 9)
      
      if (signtext_2) print(signtext_2, 64 - #signtext_2*2, signpos.y+10, 9)
      clip(args"4,4,120,120")
    end

    -- player
    if undo_left_x then
      print("\142", undo_left_x, 12, (blinkon or not died) and 6 or 0)
      print("undo", 74 - (undo_left_x - 37), 12, 6)
    end
    clearpal() -- todo: tokens - needed?
    if not died then
      if (pl.fadetype) draw_fade(stepperc, pl.fadetype)
      pl_spr = playersprites[pl_dir][pl_animclock.t] -- todo: tokens - check if pl_spr needed at all
      if not won then
        if (pl_mask) celmask(pl.pos)
        spr(pl_spr, pl.pos.x, pl.pos.y, 1, 1, pl_dir == "r")  
      end  
    end
    pal() -- todo: tokens - needed? if so clearpal()?
    if endgame then
      draw_sleepyhead()
    end

    draw_item(outplayer)
    if (not starview) draw_dissolve(fades_normal)
    
    if wonspark_pos then
      for i=0, wonspark_parts-1 do
        local t = wonspark_clock.perc
        local r,a,c = lerp(3, 36, t) - i/wonspark_parts*5, lerp(.25, 2, t) + i/wonspark_parts, i%2==0 and 10 or 7
        local wsp = wonspark_pos + pvec(r, a)
        print("\146", wsp.x-4, wsp.y-3, lerptab(fades()[c], t))
      end
    end
  else
    draw_dissolve()
  end
  if state == "heading" then
    if highest == 1 then 
      local isblu = gpath == 10
      spr(180, 14, isblu and 18 or 27)
      print("blue trail \143 beginner", 21, 18, isblu and 12 or 1)
      print("red trail \143 experienced", 21, 27, isblu and 2 or 8)
      if (transition_state == "in") pal()
    else
      print(pz.n, 64 - #pz.n*2, 20, 9)
    end
    draw_mountain()
    if (transition_animate) draw_title_foreground()
  elseif state == "title" then
    sprtb"48,10,16,14,1"
    sprtb"91,10,24"
    sprtb"91,109,24,1,1,1"
    ? args"z or x to begin,36,30,9"
    -- above fade if not transitioning in
    if (transition_state ~= "in") pal()
    draw_mountain()
    draw_title_foreground()
  elseif state == "endscreen" then
    draw_sleepyhead()
    print("the end", 52, endtext_y, 7)
  end
  -- screenflash
  if sfl_t and sfl_t > 0 then 
    sfl_t -= 1
    if sfl_t % 2 == 0 then
      clearpal()
    else
      pal(0, sfl_c, 1)
    end
  end

  -- screenshake
  if screenshake > 0 then
    screenshake -= 1
    camera(rnd(2)-1,rnd(2)-1)
  else
    camera(0,0)
  end

    -- border
  pal()
  clip()
  foreach(split("14,0,0,2,2,1_14,112,0,2,2_14,0,112,2,2,1,1_14,112,112,2,2,0,1_13,16,0,2,1_13,16,120,2,1,0,1_31,120,16,1,2_31,0,16,1,2,1,0_13,32,0,2,1_13,32,120,2,1,0,1_31,120,32,1,2_31,0,32,1,2,1,0_13,48,0,2,1_13,48,120,2,1,0,1_31,120,48,1,2_31,0,48,1,2,1,0_13,64,0,2,1_13,64,120,2,1,0,1_31,120,64,1,2_31,0,64,1,2,1,0_13,80,0,2,1_13,80,120,2,1,0,1_31,120,80,1,2_31,0,80,1,2,1,0_13,96,0,2,1_13,96,120,2,1,0,1_31,120,96,1,2_31,0,96,1,2,1,0", "_"), sprtb)
end

function draw_sleepyhead()
  palt(0b0000000000000000)
  spr(148, sleepyhead.x, sleepyhead.y, 2, 2)
  spr(7, sleepyhead.x + 6, sleepyhead.y - 3, 1, .625)
  palt()
end

function draw_mountain()
  palt(0b0001000000000000)
  map(24, 26, 0, mountain_y, 16, 10)
  palt()
  rectfill(4, mountain_y+80, 124, 124, 1) -- todo: tokens - remove
  
  if state == "heading" then 
    for i,n in ipairs(mapnodes) do
      local p, ni, z = n.p + vec(0, mountain_y), n.i, sin(i*.069)*2
      local cx, cy = p.x+z, p.y+z
      if ni == pz_i then
        rectfill(cx+3,cy-1,cx+5,cy+1,lerptab(split"12,13,2,1,2,13,12", gt.perc))
      elseif ni <= highest then -- doing: less than current solved (need to make pz_c?)
        spr((contains(solved, ni)) and gpath or 154, cx, cy-4)
      end      
    end
  end
end

function draw_title_foreground()
  palt(0b0001000000000000)
  pal(1, 0)
  map(0, 24, tmlx, 40, 12, 12)
  map(12, 24, tmrx, 40, 12, 12)
  pal()
  circfill(64, door_y + 108, 85, 0) 
  map(74, 26, 16, door_y, 12, 7)
end

function celmask(p, c)
  rectfill(p.x, p.y, p.x+7, p.y+7, c or 0)
end

function draw_item(item)
  if (item) draw_items{item}
end

function draw_items(tab, condition, selectspr) 
  for _, i in pairs(tab) do
    if (condition and condition(i) or true) draw_obj(i, selectspr)
  end
end

function draw_obj(o, selectspr)
  local p = o.pos
  if (o.mask) celmask(p)
  if (o.fadetype) draw_fade(stepperc, o.fadetype)
  spr(selectspr and selectspr(o) or o.spr, p.x, p.y)
  if (o.fadetype) clearpal()
end

function draw_items_ct(tab)
  draw_items(tab.items)
end

-- put draw operations b/w draw_fade() and a call to pal()
function draw_fade(t, fadetype)
  if (undo) fadetype = fadetype == "in" and "out" or "in"
  for c=0,15 do
    pal(c, lerptab(fades()[c], fadetype == "in" and 1-t or t))
  end
end

function fades()
  return died and fades_mono or fades_normal
end

function loadmap()
  state,steptime,shovels,changedirt,transition_state = args"puzzle,0,0,0,in"
  
  dissolve_particles,starview,willadd_shrooms,willdel_shrooms,signtab,baseobstacletab,mushtab,grasstab,dirttab,doortab,blocktab,shoveltab,snaketab,holetab,watertab,waterblocktab,thrutab,morphtab,snakecontext,pl,outsnakes,willmoveto,willblock,outblock,outplayer,goaldoor,thruitem,snakemode,died,blinkpos,blunk,won,forcefade,undo,signtext = {},containsdata("west overlook from juncture", pz.n),{},{},celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),{},{},{}

  -- load objects
  for mx = pz.x, pz.x + (pz.w - 1) do
    for my = pz.y, pz.y + (pz.h - 1) do
      local i, cell = mget(mx, my), vec(mx-pz.x, my-pz.y)

      -- stuff that grass should be under
      if (containsdata("7,43,6,84,85,86,87,88,89,90", i)) creategrass(cell)

      -- bed endgame
      if i == 148 then
        sleepyhead = cellpos(cell)
      end

      if is_dirt_type(i) then
        createdirt(cell, i)
      elseif containsdata("2,146,162,164,165,147,30,164,165,148,149", i) then
        creategrass(cell, i)
      elseif i == 43 then
        createitem(cell, i, nil, blocktab)
      elseif i == 16 then
        if containsdata("cave from juncture,cave from spring", pz.n) then
          if #solved < peakindex then
            pz.t = {"progress:", #solved.." / "..peakindex}
          else 
            creategrass(cell)
          end
        end
        if pz.t then 
          sign,signpost_pos,sign_texts = createitem(cell, i, nil, signtab),cellpos(cell),pz.t or {} -- tokens: "or {}" needed?
        end
      elseif containsdata("4,20", i) then
        createdoor(cell, i)
      elseif containsdata("3,19", i)then 
        goaldoor = createitem(cell, i, i, doortab)
        goaldoor.open, goaldoor.isgoal, goaldoor.orgspr, goaldoor.openspr = false,true,i,i == 19 and 17 or 181
      elseif containsdata("183,191,158,159,239,190,157,174,156", i) then
        local item = createitem(cell, i, 0, thrutab)
        -- emergency tokens: remove peakindex after levels are finalized? (maybe won't work with red v blue?)
        -- tokens v compression: set player coords in "links" objects i.e. door_up = ["a from b", [2,3]]
        item.topuz = pz[tostr(i)] + peakindex
      elseif i == 26 or i == 12 then
        createdirt(cell, 1)
        createmush(cell, 1, mushtab, i == 12 and 2 or 1, i)
      elseif i == 6 then
        createitem(cell, i, nil, shoveltab)
      elseif containsdata("62,63,64,65,66,67,68,69,70,71,72,73,74,75,76", i) then
        createitem(cell, i, nil, watertab)
      elseif containsdata("32,33,34,35,36,37,38,39,40,41,42,48,49,50,51,52,53,54,55,56,57,58", i) then
        -- create snakes after collecting context
        local under = containsdata("48,49,50,51,52,53,54,55,56,57,58", i) and 1 or 2
        add_snake_context(cell, under == 1 and i - 16 or i, under)
      elseif containsdata("80,81,82,83", i) then
        createitem(cell, i, nil, morphtab)
      elseif i and i ~= 0 then
        createitem(cell, i, nil, baseobstacletab)
      end
    end
  end
  createplayer(vec(pz.px, pz.py))
  createsnakes()
  check_doors(true)
  refresh_hud()
end

function createplayer(cel)
  pl.cell,pl.pos,pl.commands,pl_spr,pl_mask,pl_dir,pl_animclock = cel,cellpos(cel),{},7,true,"r",clock(2, true)
end

function create_under_snake(obj, cel)
  if obj.under == 1 then
    createdirt(cel, 1)
  elseif obj.under == 2 then
    -- todo: does it matter that grass might be created under snake morph piece (head p)?
    creategrass(cel) 
  end
end

function add_snake_context(cel, i, under, user)
  snakecontext[vecstring(cel)] = {cell=cel, index=i, under=under, user=user}
end

function createsnakes()
  for _, obj in pairs(snakecontext) do
    local cell, index = obj.cell, obj.index
    -- create snake starting at head, following body
    
    if containsdata("32,33,34,35", index) then
      create_under_snake(obj, cell)

      local body, i, lastdir, lastcel = {}, index
      while i ~= 42 do
        local dir = tb"32=d,48=d,33=l,49=l,34=u,50=u,35=r,51=r,36=u_u/d_d,37=l_l/r_r,40=r_u/d_l,41=l_u/d_r,38=r_d/u_l,39=l_d/u_r"[tostr(i)]
        if containsdata("36,37,38,39,40,41", i) then
          dir = tb(dir, "/", "_")[lastdir]     
        end

        lastcel = cell
        cell = stepcell(cell, dir)
        local bobj = snakecontext[vecstring(cell)]
        i = bobj.index
        local sprite,idir = i,dircell(lastcel,cell)
        if (i == 42) sprite = (idir == "r" or idir == "l") and 37 or 36
        add(body, createitem(cell, sprite))
        create_under_snake(bobj, cell)
        lastdir = dir
      end

      local sn = createitem(obj.cell)
      sn.mask, sn.body, sn.user = true, body, obj.user
      for b in all(body) do
        b.snake = sn
      end
      sn.snake,sn.facing = sn,dircell(sn.body[1].cell, sn.cell) or "d"
      sn.spr, pl_snake = snakespr[sn.facing], sn
      snaketab.add(sn)
    end
  end
  snakecontext = nil
end

function is_dirt_type(i)
  return containsdata("1,5,21", i)
end

function createdirt(cell, spr) 
  return createitem(cell, spr, tostr(spr), dirttab)
end

function creategrass(cell, i)
  return createitem(cell, i or 2, nil, grasstab)
end

function createitem(cell, spr, flag, ctab, mask)
  local obj = {
    cell = cell,
    pos = cellpos(cell),
    spr = spr,
    flag = flag,
    commands = {},
    mask = mask == nil and true or mask
  }
  if (ctab) ctab.add(obj)
  return obj
end

function createmush(cell, flag, ctab, t, type)
  local obj = createitem(cell, 0, flag, ctab)
  obj.t,obj.mask,obj.type = t,true,type
  return obj
end

function createdoor(cell, i)
  local obj = createitem(cell, orgspr, i == 4 and "5" or "21", doortab, false)
  obj.orgspr,obj.openspr,obj.spr = i,18,i
  return obj
end

function update_transition()
  clocktick(transition_clock)
  if transition_clock.perc == 1 then
    transition_state = nil
    if (transition_next) transition_next()
    transition_clock,transition_next= clock(.66)
  end
end

function command()
  return {
    start = function() end,
    run = function(t) end,
    next = function() end
  }
end

function batch_bit(commands, fx_name, reverse_undo)
  return function(x)
    if undo and reverse_undo then
      for i=#commands,1,-1 do
        commands[i][fx_name](x)
      end
    else
      for c in all(commands) do
        c[fx_name](x)
      end
    end
  end
end

function batch_command(commands, reverse_undo)
  local c = command()
  c.start = batch_bit(commands, "start", reverse_undo)
  c.run = batch_bit(commands, "run", reverse_undo)
  c.next = batch_bit(commands, "next", reverse_undo)
  return c
end

function getshovel_command(nextshovel)
  local c, sp = command(), nextshovel.pos
  c.start = function()
    if undo then
      shovels -= 1
      shoveltab.add(nextshovel)
    else  
      playsfx"18"
    end
  end
  c.run = function(t)
    nextshovel.pos = lerpvec(undo and shovtarg or sp, undo and sp or shovtarg, t, smooth)
  end
  c.next = function()
    if not undo then 
      shoveltab.del(nextshovel.cell)
      shovels += 1
    end
  end
  return c
end

function setdoor_command(door, close)
  local c, prevspr, prevopen = command(), door.spr, door.open
  c.start = function()
    if undo then
      door.open, door.spr = prevopen, prevspr
    else
      door.open, door.spr = not close, close and door.orgspr or door.openspr
      if door.open ~= prevopen then
        if close then
          playsfx"13"
        else
          playsfx"5"
        end
      end
    end
  end
  return c
end

function thrudoor_command(incell)
  local c, _, outdoor  = moveplayer_command(stepcell(incell, "d"), incell), doorpair(incell)
  local outdoor_cell, n = outdoor.cell, c.next
  local outdoor_landing, outplayer_local, prevdir = stepcell(outdoor_cell, "d"), createitem(outdoor_cell, 8, nil, nil, false), pl_dir
  outplayer_local.fadetype = "in"
  c.start = function()
    pl_dir, outplayer, invertcontrols, pl_mask = "u", outplayer_local, not undo, false
  end
  c.next = function()
    n()
    pl_dir = undo and prevdir or "d"
    if (not undo) pl.cell, pl.pos, pl_mask = outdoor_landing, cellpos(outdoor_landing), not watertab.item(outdoor_landing)
    outplayer = nil
  end
  return batch_command{c, movecell_command(outplayer_local, outdoor_cell, outdoor_landing), fade_command(pl)}
end

function add_snake_commands()
  local kills = {}
  for _, snake in pairs(snaketab.items) do
    if not snake.willdie then
      local at = stepsnake(snake, true)
      -- todo: outcel for door snake head kill snake going thru door bug? (line 1569)
      local segs, move, outcel = is_snake(at, true, snake, true), movesnake_command(snake, snake.cell, stepsnake(snake))
      addbatch(move)
      for seg in all(segs) do
        if (not seg.snake.willdie) add(kills, {seg, snake, at})
      end
    end
  end
  for p in all(kills) do
    local excludecel = p[3]
    for pp in all(kills) do
      if (p[2] == pp[1].snake) excludecel = nil
    end
    addbatch(killsnake_command(p[1].snake, false, excludecel))
  end
end

function stepsnake(snake, usedoors)
  local snakecell, snakefacing = snake.cell, snake.user and userdir or snake.facing 
  local behind = stepcell(snakecell, turn(turn(snakefacing, 1), 1))

  local moveto = behind
  for cel in all{stepcell(snakecell, snakefacing), stepcell(snakecell, turn(snakefacing, 1)), stepcell(snakecell, turn(snakefacing, 2))} do
    if not is_snake_obstacle(snake, cel) then 
      moveto = cel
      break
    end
  end

  if usedoors then
    local indoor, outdoor = doorpair(moveto)
    if indoor and indoor.open and not indoor.isgoal and dircell(snake.cell, indoor.cell) == "u" and not is_snake_obstacle(snake, stepcell(outdoor.cell, "d")) then
      moveto = stepcell(outdoor.cell, "d")
    end
  end
  return moveto
end

function waterblock(cel)
  return waterblocktab.item(cel)
end

function is_snake_obstacle(snake, cell) 
  local willdig,obstacle,indoor,outdoor = willdig_at == cell and digtype(cell) == 22, #items(cell) == 0 or blocktab.item(cell) or shoveltab.item(cell) or baseobstacletab.item(cell) or signtab.item(cell) or thrutab.item(cell), doorpair(cell)
  if (watertab.item(cell) and not waterblock(cell)) obstacle = true
  if indoor then
    if indoor.open and not indoor.isgoal then 
      obstacle = dircell(snake.cell, indoor.cell) ~= "u" 
      obstacle = obstacle or is_snake_obstacle(snake, stepcell(outdoor.cell, "d"))
    else 
      obstacle = true
    end
  end
  if (willblock) obstacle = obstacle or willblock == cell
  return obstacle or willdig
end

function is_snake(at, stepahead, exclude, checkall)
  local results = {}
  for _, s in pairs(snaketab.items) do
    if s ~= exclude then
      local cel = stepahead and stepsnake(s, true) or s.cell
      if cel == at then 
        if checkall then
          add(results, s)
        else
          return s
        end
      end

      for i,b in ipairs(s.body) do
        local bcel = b.cell
        if (stepahead) bcel = i > 1 and s.body[i-1].cell or s.cell
        if bcel == at then
          if checkall then
            add(results, b)
          else
            return b
          end
        end
      end
    end
  end
  if (checkall) return results
end

function turn(dir, cw)
  return tb"u=r;l,r=d;u,d=l;r,l=u;d"[dir][cw]
end

function add_grow_commands(shrooms)
  for m in all(shrooms) do
    local commands = {}
    if not willdel_shrooms[vecstring(m.cell)] then
      if m.t == 0 or (m.t == 1 and m.type == 12) then
        add(commands, growmush_command(m))
      else -- expand colony
        for d in all(split"u,d,l,r") do
          local at = stepcell(m.cell, d)
          local _,outd = doorpair(at)
          if (outd and outd.open and d == "u") at = stepcell(outd.cell, "d")
          local existing = items(at)[1] 
          local blocked = #items(at) ~= 1 or
            (existing and not is_dirt_type(existing.spr)) or
            (willmoveto and at == willmoveto) or 
            (willadd_shrooms[vecstring(at)] ~= nil) or 
            is_snake(at) or 
            is_snake(at, true)

          local ispoison = existing and existing.type == 12
          local squashpoison, valid = ispoison and m.type == 26, existing and not blocked
          if valid or squashpoison then
            if (squashpoison) add(commands, delmush_command(existing))
            add(commands, addmush_command(at, m.type, "8"))
          end
        end
      end
    end
    for c in all(commands) do
      addbatch(c)
    end
  end
end

function addmush_command(at, type, sf)
  local c, m = command(), createmush(at, dirttab.item(at).flag, nil, 0, type)
  willadd_shrooms[vecstring(at)] = m

  c.start = function()
    if not undo then 
      if (sf) playsfx(sf, true)
      mushtab.add(m)
    end
  end
  c.next = function()
    if (undo) mushtab.del(at)
  end

  return c
end

function growmush_command(mush)
  local c = command()
  c.start = function()
    mush.t = undo and mush.t - 1 or mush.t + 1
    if (not undo) playsfx("8", true)
  end
  return c
end

function playsfx(d, shy)
  if (shy and stat(19) >= 0) return
  sfx(tonum(d), 3)
end

function delmush_command(mush, pick)
  willdel_shrooms[vecstring(mush.cell)] = mush
  local c,t = command(),mush.t
  c.start = function()
    if not undo then
      if (pick) playsfx("3", true)
      mushtab.del(mush.cell)
    end 
  end
  c.next = function()
    if undo then
      local d = dirttab.item(mush.cell)
      mushtab.add(mush)
    end
  end
  return c
end

function fade_command(obj, fadein)
  local c = command()
  c.start = function ()
    obj.fadetype = fadein and "in" or "out"
  end
  c.next = function()
    obj.fadetype = nil
  end
  return c
end

function win_command(door)
  local create_windelay = function()
    windelay = clock(door and 1 or .01)
  end
  local c = command()
  c.next = function()
    if door then
      playsfx"6"
      wonspark_pos,wonspark_clock,wonspark_parts = door.pos+vec(4,4),clock(2.5),10
      if (not contains(solved, pz_i)) add(solved, pz_i)
    end
    won,transition_state,transition_next = true,"out",create_windelay
    menuitem(1) -- clear reset puzzle
  end
  return c
end

function die_command() 
  local c = command() 
  c.next = function()
    died = not undo
  end
  c.run = function(t)
    undo_left_x = not undo and lerp(37, 51, stepperc, sineout)
  end
  return c
end

function doorpair(cell)
  local a = doortab.item(cell) 
  if a then
    for _, d in pairs(doortab.items) do 
      if (d.flag == a.flag and d ~= a) return a, d
    end
    return a
  end
end

function movepushblock_command(pushblock, to)
  local from = pushblock.cell
  local c = movecell_command(pushblock, from, to)
  local n, indoor, outdoor, outdoor_stepcell, outblock_local = c.next, doorpair(to)
  if indoor then
      -- todo crash: outdoor can be nil when pushing 2 blocks up against a goal door
    outdoor_stepcell, outblock_local = stepcell(outdoor.cell, "d"), createitem(outdoor.cell, 43, nil, nil, false)
    outblock_local.fadetype = "in"
    addbatch(movecell_command(outblock_local, outdoor.cell, outdoor_stepcell))
  end
  
  local tocell, fromwater = outdoor_stepcell or to, watertab.item(from)
  local towater, towaterblock = watertab.item(tocell), waterblock(tocell)

  c.start = function()
    if (indoor) outblock, inblock = outblock_local, indoor and pushblock
    if towater then 
      if not indoor then
        pushblock.mask = false
        if (not towaterblock) pushblock.spr = 46
      end
      if undo and not blocktab.item(tocell) then
        waterblocktab.del(tocell)
        pushblock.cell = tocell
        blocktab.add(pushblock)
      end
      if (not towaterblock) pushblock.inwater = not undo
    elseif not undo then
      playsfx"20"
    end
    if (fromwater and undo) pushblock.mask = false
  end

  c.next = function()
    n()
    if towater and not undo and not towaterblock then
      blocktab.del(from)
      createitem(tocell, 46, nil, waterblocktab, false)
    else
      blocktab.move(pushblock, undo and tocell or from, undo and from or tocell)
      if undo then
        if (towater) pushblock.spr = 43
        if (not fromwater) pushblock.mask = true
      else
        pushblock.cell, pushblock.pos, pushblock.mask = tocell, cellpos(tocell), not towater
        local mush = mushtab.item(tocell)
        if (mush) mushtab.del(mush.cell)
      end
    end
    if (indoor) outblock, inblock = nil
  end

  local mush = mushtab.item(tocell)
  if (mush) addbatch(delmush_command(mush))
  if (indoor) addbatch(fade_command(pushblock))
  if (towater and not towaterblock) addbatch(splash_command(pushblock, tocell))
  return c
end

function signtarget(text)
  return vec(64 - #text*2, 15)
end

function moveplayer_command(from, to)
  local c, orgdir, orgobs = movecell_command(pl, from, to), pl_dir, {}
  local n, r, lastmask, d, towater, fromwater, towaterblock, tomorph, endgame_local = c.next, c.run, pl_mask, dircell(from, to), watertab.item(to), watertab.item(from), waterblock(to), morphtab.item(to), containsdata("148,149", items(to)[1].spr)

  c.start = function()
    if not undo and tomorph then 
      playsfx"22"
      screenshake = stepdur_org
    end
    pl_dir = d
    clocktick(pl_animclock, undo and -1 or 1)
    if (towater or (goaldoor and goaldoor.cell == to) or (fromwater and undo)) pl_mask = false
  end
  c.next = function()
    n()
    if undo then 
      pl_dir, pl_mask = orgdir, lastmask
    elseif fromwater and not towaterblock then 
      pl_mask = true
    elseif endgame_local then
      endgame, pl.pos = 1, vec(1000,1000)
    end

    if tomorph then 
      if undo then
        snaketab.del(to)
        snakemode, snakecontext = nil
        for obs in all(orgobs) do
          baseobstacletab.add(obs)
        end
      else
        snakemode, snakecontext = true, {}
        add_snake_context(to, tomorph.spr-48, nil, true)

        for _,val in pairs(baseobstacletab.items) do
          if containsdata("84,85,86,87,88,89,90", val.spr) then
            add_snake_context(val.cell, val.spr-48, nil)
            add(orgobs, val)
            baseobstacletab.del(val.cell)
          end
        end

        createsnakes()          
      end
      
      -- move player out of the way or (undo) restore player position
      pl.pos, pl.cell = undo and cellpos(from) or vec(1000,1000), undo and from or vec(100,100)
    end
  end

  if (tomorph or endgame_local) fadepl()
  return c
end

function movecell_command(obj, from, to)
  local c = move_command(obj, cellpos(from), cellpos(to))
  c.next = function()
    obj.cell = undo and from or to
  end
  return c
end

function move_command(obj, from, to)
  local c = command()
  c.run = function(t) 
    obj.pos = lerpvec(undo and to or from, undo and from or to, t, sineout)
  end
  return c
end

function movesnake_command(snake, from, to)
  local indoor, outdoor, outdoor_stepcell, outsnake_local, last_morph_spr = doorpair(to)
  if indoor and not indoor.isgoal then
    outdoor_stepcell, outsnake_local = stepcell(outdoor.cell, "d"), createitem(outdoor.cell, 34, nil, nil, false)
    outsnake_local.fadetype = "in"
    addbatch(movecell_command(outsnake_local, outdoor.cell, outdoor_stepcell))
  end
  local tocell = outdoor_stepcell or to
  local c, waterblockto, morphto = move_command(snake, cellpos(from), cellpos(to)), waterblock(tocell), snake.user and morphtab.item(to)
  if (morphto) last_morph_spr = morphto.spr

  local n, lastbody, dir, lastspr, lastfacing, mush = c.next, snake.body, dircell(from, to), snake.spr, snake.facing, mushtab.item(tocell) or willadd_shrooms[vecstring(tocell)]

  local bodies, overlap, willmorph = {[vecstring(to)]=true}
  
  c.start = function()
    if outsnake_local then 
      add(outsnakes, outsnake_local)
    end
    if undo then
      snake.body, snake.facing = lastbody, lastfacing
      if (waterblock(from) or indoor) snake.mask = false
    else
      if (waterblockto or indoor) snake.mask = false
      local nextbody = {}
      for i,_ in ipairs(lastbody) do
        local seg = lastbody[i-1]
        if i == 1 then 
          seg = createitem(from, bodylut[dir..(dircell(from, lastbody[1].cell) or "u")])
          seg.snake = snake
        end
        if (waterblock(seg.cell)) seg.mask = false
        add(nextbody, seg)
      end
      snake.body, snake.spr = nextbody, dir and snakespr[dir] or 34
    end
    snake.cell = undo and from or tocell -- set the cell immediately, for particles

    -- check for morph overlap
    for b in all(snake.body) do
      if bodies[vecstring(b.cell)] then 
        overlap = true
        break
      end
      bodies[vecstring(b.cell)] = true
    end
    if morphto and not overlap  then
      playsfx"22"
      screenshake, willmorph = stepdur_org, true -- todo: overload screenshake to mean willmorph?
    end
  end

  c.next = function()
    n()
    snaketab.move(snake, undo and tocell or from, undo and from or tocell)
    if undo then
      snake.spr = lastspr
      if (not waterblock(from)) snake.mask = true
    else
      if (outsnake_local) snake.spr = 34
      if (not waterblockto) snake.mask = true
    end
    if outsnake_local then 
      del(outsnakes, outsnake_local)
    end
    if (not undo) snake.facing = outsnake_local and "d" or dircell(snake.body[1].cell, snake.cell)

    -- "dock"
    if willmorph then
      if undo then
        snakemode, pl.pos, pl.cell, pl_snake = true, vec(1000,1000), vec(100,100), snake
        snaketab.add(snake)
        for _,val in pairs(baseobstacletab.items) do
          if containsdata("84,85,86,87,88,89,90", val.spr) then
            baseobstacletab.del(val.cell)
          end
        end
        morphto.spr = last_morph_spr
      else
        pl.pos, pl.cell, snakemode = cellpos(to), to
        for b in all(snake.body) do
          createitem(b.cell, b == snake.body[#snake.body] and 90 or b.spr+48, nil, baseobstacletab)
        end
        snaketab.del(to)
        morphto.spr = snake.spr+48
      end
      check_doors(true)
    end
  end

  if mush then
    addbatch(delmush_command(mush, true))
  end

  return c, outdoor_stepcell
end

function clock(dur, asframes)
  -- if asframes == true, dur is frames, instead of seconds
  local maxt = asframes and dur or ceil(dur * 30)
  return {
    maxt = maxt,
    t = 1,
    perc = 1 / maxt
  }
end

function clocktick(c, increment)
  if (not c) return
  c.t = wrap(c.t + (increment and increment or 1), 1, c.maxt, true)
  c.perc = c.t / c.maxt
end

function contains(collection, item)
  for i in all(collection) do
    if (i == item) return true
  end
  return false
end

function containsdata(collection, item)
  return contains(split(collection), item)
end

-- todo: tokens - smaller version ?
function wrap(n, low, high, int)
  if (n < low) return high - (int and (low-n)-1 or low-n)
  if (n > high) return low + (int and (n-high)-1 or n-high)
  return n
end

function cellpos(cell)
  return vec((128 - pz.w * 8) / 2, (128 - pz.h * 8) / 2) + vec(cell.x * 8, cell.y * 8)
end

function stepcell(c, dir)
  return vec(
    dir == "l" and c.x - 1 or dir == "r" and c.x + 1 or c.x, 
    dir == "u" and c.y - 1 or dir == "d" and c.y + 1 or c.y
  )
end

function dircell(from, to)
  if (from.y == to.y) return from.x > to.x and "l" or from.x < to.x and "r"
  if (from.x == to.x) return from.y > to.y and "u" or from.y < to.y and "d"
end

function celltab()
  local tab = {items={},flagmap={}}
  tab.add = function(item)
    tab.items[vecstring(item.cell)] = item
    local key = item.flag or "none"
    local existing = tab.flagmap[key]
    if existing then
      add(existing, item) 
    else
      tab.flagmap[key] = {item}
    end
  end
  tab.del = function(cell)
    local item = tab.item(cell)
    if (item) del(tab.flagmap[item.flag], item)
    tab.items[vecstring(cell)] = nil
  end
  tab.move = function(obj, from, to)
    tab.items[vecstring(to)] = obj
    if (obj == tab.item(from)) tab.items[vecstring(from)] = nil
    if (obj.pos) obj.pos = cellpos(to)
  end
  tab.item = function(cell)
    return tab.items[vecstring(cell)]
  end
  tab.flaggeditems = function(flag)
    return tab.flagmap[flag or "none"]
  end
  tab.itemcount = function(flag)
    local result = tab.flaggeditems(flag)
    return result and #result or 0
  end
  return tab
end

function filterpairs(tab, f)
  local r = {}
  for k, v in pairs(tab) do
    if (f(k, v)) add(r, v)
  end
  return r
end

-- todo: tokens - add body of snakes into this, and could prob remove a bunch of is_snake + #items(at) code?
function items(cell)
  local result = {pl}
  if (cell != pl.cell or died) result = {}
  for tab in all{baseobstacletab, mushtab, grasstab, dirttab, doortab, blocktab, snaketab, shoveltab, watertab, waterblocktab, signtab, thrutab, morphtab} do
    local item = tab.item(cell)
    if (item) add(result, item)
  end
  return result
end

function mushspr(item)
  return tb"12=11;27;12,26=10;26"[tostr(item.type)][item.t+1]
end

function btnd(b) 
  if (not b) return (btn() > 0 and lastdowns[6] == 0 )
  return (btn(b) and (lastdowns[b] == false))
end

vector = {}
function vec(x, y)
  local t = {x=x, y=y}
  setmetatable(t, vector)
  return t
end

function vector.__add(a, b)
  return vec(a.x + b.x, a.y + b.y)
end

function vector.__eq(a, b)
  return a.x == b.x and a.y == b.y
end

function pvec(r, ang)
  return vec(cos(ang) * r, sin(ang) *r)
end

function lerp(a, b, t, e)
  if (e) t = e(t)
  return a + (t * (b - a))
end

function lerpvec(a, b, t, e)
  return vec(lerp(a.x, b.x, t, e), lerp(a.y, b.y, t, e))
end

function lerptab(tab, t)
  -- todo: use generic lerp to allow easing?
  return tab[max(1, ceil(t * #tab))]
end

function sineout(t) 
  return sin(-t/4)
end

function sinein(t)
  return 1-cos(-t/4)
end

function smooth(t)
  return t^3*(t*(6*t-15)+10)
end

-- function log(s)
--   printh(s, "mush")
-- end

-- function boolstring(b) 
--   if (b ~= nil) return b and "true" or "false"
--   return "nil"
-- end

-- function nilstring(s)
--   return s ~= nil and s or "nil"
-- end

function vecstring(v)
  return (v and v.x or "nil")..":"..(v and v.y or "nil")
end

function args(s, d)
  return unpack(split(s, d))
end

__gfx__
00000000000000000000000009009009000999000000000000007770000222000022000000220000000000000000000000000000000000100010000000011111
00000000000000000000000009999999009444900000000000076665002222200222200002222000000000000000000000770000000010010010110010100001
00700700040004000000000009889889094494490000400000766665022222220222220002222200000000000000000007777000100001010001001010006c01
00077000000000000000b030098898890949944900044000000666650d222002202dd2002022220000ccc000000000000020000000111100011000000100c101
000770000040004000000300098a9a890949444900040000004266500dd00000000dd000000220000ccccc000077700000807770000000000000000000010001
00700700000000000b0300000982928909449949000044000421050000000d0000d0000000000d00000100000002000000800200000000000000000000000010
0000000004000400003000000988988909444449000000004210000000d2200000022d0000d22000000c00000008077000080800000000000000000000001000
00000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000110
00000000099999990009990009999999000999000000000000000000000000002000000020000000000000000000000000000000000000000000000000000000
000000000c90009c00900090002c9c2000944490000000000004400000002222222000002220000000cccc000000000000000000000000000000000000000100
044444400990009909000009099c9c990944444900000000004224000022222002222000022220000cccccc00000000000000003000000000000000000000010
04222240090000090900000909cc9cc9094994490004400004211240022222000022220000222200000100000077700000000003000000000000000000000010
04444440090000090900000909ca9ac90944994900004400041001400d22d000002dd200002222000000c0000002000000000030300000000000000000000100
00022000090000090900000909c292c90949444900040000004004000dd00000000ddd0000d220000000c0000008087000000333330000000000000000001011
00044000090000090900000909cc9cc90944444900000000000440000022d00000d2200000022d00000c00000008080000000330330000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003003003000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003333333000000000000000000110
00088000000800000008800000008000000880000000800000000000000000000008800000088000000000000000000000030330330300000000000000000001
00a88a0000888a000088880000a88800008888000088880000882800008288000028880000888200000000000999999000003003003000000199991000001100
00888800088888800888888008888880008888000888888008888200002888800888880000888880080000800900009000033333333300000911114000001010
08888880088888800088880008888880088888800888888008888800008888800888820000288880088888800999999000003330333000000999444000001000
0088880000888a0000a88a0000a88800008888000088880000288800008882000088280000828800080000800900009000000001000000000911114000001000
00022000000200000002200000002000000220000000200000088000000880000000000000000000000000000999999000000004000000000144441000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
a00000a0a000a00aaa00a000a0aaaa000aaa000aaa00a00000a0000000a00000a00aaa00a000a0a0000a0aaaaa000aaa000a0a0000a000004111111111111114
aaa0aaa0aa0aa0aaaaa0aa0aa0aaaaa0aaaaa0aaaaa0aaa0aaa0000000aaa0aaa0aaaaa0aa0aa0aa00aa0aaaaaa0aaaaa0aa0aa00aa000004111111111111114
aaaaaaa0aa0aa0aa0aa0aa0aa0aa0aa0aaaaa0aaaaa0aaaaaaa0000000aaaaaaa0aaaaa0aa0aa0aaa0aa00aaaaa0aa0aa0aa0aaa0aa000000411111111111140
99999990990990999000999990999900990990990990999999900000009999999099099099099099999900099000999990990999999000000411111111111140
99090990990990009990999990999990990990990990990909900000009909099099099099099099999900099000999990990999999000000411111111111140
99000990999990990990990990990990999990999990990009900000009900099099999099999099099900099000990990990990999000000411111111111140
99000990999990999990990990990990999990999990990009900000009900099099999099999099009900099000990990990990099000004111111111111114
99000090099900099900900090900900099900099900900000900000009000009009990009990090000900090000900090900900099000004111111111111114
41111114000000004400004411111111000000444400000041111111111111144111111444000000000000444400004411111111333011113333330033333333
41111114000440001144441111111111000444111144400041111111111111144111111411444000000444111144441111111111333011113333001133333333
04111140004114001111111111111111004111111111140004111111111111400411114011111400004111111111111111111111330111113300111133333333
04111140041111401111111111111111041111111111114004111111111111400411114011111140041111111111111111111111330111110011111133333333
04111140041111401111111111111111041111111111114004111111111111400411114011111140041111111111111111111111301111111111111133333300
04111140041111401111111111111111041111111111114000411111111114000041140011111400004111111111111111111111301111111111111133330011
41111114411111141144441111444411411111111111111400044411114440000004400011444000000444111111111111111111011111111111111133001111
41111114411111144400004444000044411111111111111400000044440000000000000044000000000000441111111111111111011111111111111100111111
0d01110d0d01110d0d00200d0d01110d0dd020dd0ddddddd0ddddddd0ddddddd0dd020dd0dd020dd0ddddddd9900000001999888889998892988999888889991
000000000000000100000000010000010d00200d0d00000d0d00000d0d00000d0d00200d0d00200d0d00000d9900000001999889998888892988888999889991
010222010002220101022201010222000d00200d000000000000000d0d0000000000200d0d0020000d02220d9900000001999898888888892988888888989991
010282010202820101028201010282020d00200d022222220222200d0d0022220222200d0d0022220d02020d9900000000999898888888892988888888989991
010222010002220101022201010222000d00200d000000000000200d0d0020000000000d0d0000000d02220d9900000000199889888888892988888889889910
000000000000000000000000000000010d00200d0d00000d0d00200d0d00200d0d00000d0d00000d0d00000d9000000000199888988888892988888898889910
0d00200d0d01110d0d01110d0d01111d0dd020dd0ddddddd0dd020dd0dd020dd0ddddddd0ddddddd0ddddddd0900000000199888988888892988888898889910
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000009000000000199889888888892988888889889910
00333333333333000333333333333330111103333330111100033333333330003333333333333333333333300333333300199898888888892988888888989910
11003333333300111033333333333301111110333301111111100033330001113333333333333333333333300333333300019898888888892988888888989100
11110333333011111103333333333011111111033011111111111103301111113333333333333333333333011033333300019898888889892989888888989100
11111033330111111103333333333011111111033011111111111110011111113333333333333333333333011033333300019898888899892989988888989100
11111103301111111110333333330111111111100111111111111111111111110000333333330000333330111103333300199898888898892988988888989910
11111103301111111110333333330111111111100111111111111111111111111111003333001111333301111110333300199898888899892989988888989910
11111110011111111110333333330111111111100111111111111111111111111111110330111111330011111111003300199898888882892982888888989910
11111110011111111110333333330111111111100111111111111111111111111111111001111111001111111111110000199898888888892988888888989910
33333333333333333131313111110111111111111111111111111111111111111111110011111111333333303333333001199898888888892988888888989910
33333333333333331313131311100011100111111111100111111111011111101111001111111111333333013333333001999898888888892988888888989991
33333333333333331111111111000001111000000000011100100010100000011110111111111111333330113333330101999889888888892988888889889991
33333300003333331111111111000001111111111111111110011001111111111101111111111111333301113333330101999888999998892988999998889991
33333011110333331111111110000000111111111111111111111111111111111011111111111111333011113333301101999888888889892989888888889991
33330111111033331111111110000000111111111111111111111111111111111011111111111111330111113333301101999888888888892988888888889991
33301111111103331111111111000001111111111111111111111111111111110111111111111111301111113333011101999999999999992999999999999991
33301111111103331111111111110111111111111111111111111111111111110111111111111111011111113333011101111111111111111111111111111111
e1e1e1e1e1e1e19e00000000000000d4e1e1e1e1e1e1e1e100001697473797679797675797060000000000a00000000000520000000000002c00000000000000
00000000003c00000000005e00008e00000000deee0000000000000000000000000000000000000000000000000000000000000000000000000000000d1d0000
e1e1e1e1e1e1e1e1ae0000000000b7e1e1e1e1e1e1e1e1e100b7979797979797979797973797ae00000000908070000000423222000000002e3d3d3d3d3d3d3d
3d3d3d3d3d3e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000eac1d10c7b1c00
e1e1e1e1e1e1e1e19e0000000000d4e1e1e1e1e1e1e1e1e100d43797979797979797979797379e00000000000000600000000000120000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020c2d21001c1d1
e1e1e1e1e1e1e1e1e1e1e1e105e1e1e1e1e1e1e1e1e1e1e1a63797979797979797979797979797b6000010203040500000000000020000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002020202020c2d2
00000000000000000111111100022200000004477744000000000000000000000000000000000000000000000000000000000000040404040240000000000000
00000000000000000100000100222220002204077704022000000000000000000000000000555000000000000000000001111111000000000242400000000000
00000000000000000101100002222222022004000004002200000000000000000000000005555500000000000000000001000001040000040242424000000420
0000000000000000010100300d222002020004088804000200099900000000000000000005055500000040000000000001011101000000000242424000042420
000000000000000000010d000dd00000020d040888040d0209911199000000000000000005550000000404000000000000011100040000040242424001242420
00000000000000000b00000100000000020d040888040d02091ccc19000000000000000005003300000040000000000001010001000000000002424001242420
00000000000000000030111100000000020d040000040d02091ccc19000000000000000055552050000000000000200001000000040000040000024001242420
00000000000000000000000000000000020d044444440d02091ccc19000000000000000000000000000000000020202000000000000000000000000000000000
00000000000000000111111104444444020d000000000d02091ccc19000000000000000000000000000000002220202220000000000000000000000004444444
00000000000000000100000104004004020d022000220d02091ccc19000000000000000000055000000000222220202222200000000000000000000100000000
00000000000000000101110104004004020d002222200d02091ccc19000000000000000000555500000022222220202222222000000000000100010004444444
00000000000000000101110104444444020dd0000000dd02091ccc19000000000000000000550500002222222220202222222220000000000001110004444444
000000000000000001011101040040040200ddddddddd002091ccc19000000000000000000555500222222222220202222222222200000000101110100000000
00000000000000000100000104004004022000000000002200911190000000000000000005505500222222222220202222222222200000000100000104444444
00000000000000000111111104444444002222222222222009099909000000000000000005030550222222222220202222222222200000000111111104444444
00000000000000000000000000000000000000000000000000000000000000000000000000000000222222222220202222222222200000000000000000000000
01111111000000111110000001111111900000000988988900000000555555550000000000000000222222222220202222222222200000000444444404444444
0199999910000199999100001999999199000000099999990000000051000015d066660d00055500222222222220002222222222200000000400000404100014
00199999911119999999111199999910999000000900000900009000510000155066660500505500222222222200400222222222200000000401110404100014
00199989999999992999999999899910990000000900000900099900510000156666666600555100222222220044444002222222200000000401110404100014
00199889988899892989988899889910900000000900000900999990510000156666666600555500222222000000000000022222200000000401110404100014
00199898888889892989888888989910000000000900000900009000510000105566665500555500222200444444444444400222200000000401110404100014
011998988998898929898899889899100000000009000009000000005100020bd066660d05555550220044444444444444444002200000000401110404100014
01999889988889892989888899889991000000000000000000000000000000305055550500000000000000000000000000000000000000000000000000000000
00000000000000000400000000000040000000000000000000000000000000110000000000000000000000000000000000000000000000000000000000000000
00066600006660000400000000000040000000000000000000000000000011c10000000000000000000000000000000000000000000000001110000000000000
006766d006766d0004000000000000400000000000111100000000000011cccc100000000000000000000000000000000110000000000001ccc1000000000000
006666d0066666d004000000000000400000000001cccc100000000001ccccccc10000000000000000000000000000001cc100000000011ccccc100000000000
0666d100066666500400000000000010000000001cccccc10000000000111cc110000000000000000000000000111111cccc100000011cccccccc10000000000
06666d1006666d10010000000000004000000001cccccccc110000000000011100000000000001100000000011cccccc1cccc100011ccccccccccc1000000000
005551000055110004000000000000400000001ccccccccccc11000000001c100000000000011cc100000011ccccccccc11110111cccccccccccccc111000000
00000000000000000400000000000040000001cccccccccccccc11000000010000000000001ccccc100001ccc11ccccccc1001cccccccccccccccccccc111000
0000000000000000000000000000000000011ccccccccccccccccc11000000000111000001ccccccc1001cccccccccccccc11cccccccccccccccccc11cccc100
00000000666000000000000000000000001cc11cccccccccccccccc1000000011ccc1100001111111000011111ccccc11110011ccccccccccccc111ccccc1000
000000067666d000144444440000000001ccccc111cccccccccccc100000011ccccccc1000001c100000000000111110000000011111ccccccccccccccc10000
000066d566666d000000000000000000001ccccccccccccccc11110000001cccccccccc110000110000000000001c100000000000000111ccccccc1111100000
00067666d6666d000000000000000000000111ccccccccc1110000000001ccccccccccccc1000000000000000001c10000000000000000011111110000000000
006666665666d100000000001444444400000011111111100000000000001111cccccc1110000000000000000001c1000000000000000001c100000000000000
0005661011d5100000000000000000000000000000001c100000000000000000111111000000000000000000001c10000000000000000001c100000000000000
000000000000000000000000000000000000000000001c1000000000000000001c1000000000000000000000001c10000000000000000001c100000000000000
044000000000044004000000000000400000000000001cc1333333330033333301c100001111033303333333001c10000333333300000001c100000000000000
4004004444004004040000000000004000000000000011c1333333331100333301c100001111033303333333000110001033333300000001c100000000000000
4044040000404404010000000000001000000000000001c133333333111100331c100000111110331033333300000000110333330000001c1000000000000000
4000040000400004044440000004444000000000000001c1333333331111110001000000111110331033333300000000111033330000001c1000000000000000
0444400000044440400004000040000400000000000001c10033333311111111000000001111110311033333000000001111033300000001c100000000000000
010000000000001040440400004044040000000000001c101100333311111111000000001111110311033333000000001111103300000001c100000000000000
04000000000000404004004444004004000000000001cc1011110033111111110000000011111110111033330000000011111103000000001100000000000000
04000000000000400440000000000440000000000000110011111100111111110000000011111110111033330000000011111110000000000000000000000000
__label__
11111000000001000000001000100000000000100010000000000010001000000000001000100000000000100010000000000010001000000010000000011111
10000101001101000000100100101100000010010010110000001001001011000000100100101100000010010010110000001001001011000010110010100001
10c60001010010001000010100010010100001010001001010000101000100101000010100010010100001010001001010000101000100100001001010006c01
101c001000000110001111000110000000111100011000000011110001100000001111000110000000111100011000000011110001100000011000000100c101
10001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010001
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
11010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001011
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000a00000a0a000a00aaa00a000a0aaaa000aaa000aaa00a00000a0000000a00000a00aaa00a000a0a0000a0aaaaa000aaa000a0a0000a00000000000
0010000000aaa0aaa0aa0aa0aaaaa0aa0aa0aaaaa0aaaaa0aaaaa0aaa0aaa0000000aaa0aaa0aaaaa0aa0aa0aa00aa0aaaaaa0aaaaa0aa0aa00aa00000000100
0100000000aaaaaaa0aa0aa0aa0aa0aa0aa0aa0aa0aaaaa0aaaaa0aaaaaaa0000000aaaaaaa0aaaaa0aa0aa0aaa0aa00aaaaa0aa0aa0aa0aaa0aa00000000010
01000000009999999099099099900099999099990099099099099099999990000000999999909909909909909999990009900099999099099999900000000010
00100000009909099099099000999099999099999099099099099099090990000000990909909909909909909999990009900099999099099999900000000100
11010000009900099099999099099099099099099099999099999099000990000000990009909999909999909909990009900099099099099099900000001011
00010000009900099099999099999099099099099099999099999099000990000000990009909999909999909900990009900099099099099009900000001000
00000000009900009009990009990090009090090009990009990090000090000000900000900999000999009000090009000090009090090009900000000000
01100000009900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009900000000110
10000000009900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009900000000001
00110000009900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009900000001100
01010000009900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009900000001010
00010000009900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009900000001000
00010000009000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000900000001000
00000000000900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009000000000000
01000000009000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000900000000010
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
11010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001011
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
00110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000010101010101010100000000000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000001101010101010101010000000000000000000000000000000000000000000000000000010
00000000000000000000000000000000000000000000000000000011111111111111111111000000000000000000000066666660060000000000000000000000
00100000000000000000000000000000000000000000000000000011111111111111111111000000000000000000066666666666666660000000000000000000
01000000000000000000000000000000000000000000000000000111111111111111111111100000000000000066666666666666666666660000000000000010
01000000000000000000000000000000000066600000000000000111111111111111111111100000000000006666666666666666666666666600000000000010
00100000000000000000000000000000666666666000000000000111111111111111111111106000000000666666666666666666666666666000000000000000
11010000000000000066666000000066666666666660000000060111111111111111111111106660000006666666666666666666666666660000000000000011
00010000000000000006666660000666666666666666000066601111111111111111111111110666000066666666666666666666666666600000000000000000
00000000000000000000666666606666666666666666606666601111111111111111111111110666660666666666666666666666666666000000000000000000
01100000000000000000066666666666666666666666666666011111111111111111111111111066666666666666666666666666666660000000000000000010
10000000000000000000006666666666666666666666666666011111111111111111111111111066666666666666666666666666666600000000000000000001
00110000000000000000000666666666666666666666666660111111111111111111111111111106666666666666666666666666666000000000000000000000
01010000000000000000000066666666666666666666666660111111111111111111111111111106666666666666666666666666660000000000000000000010
00010000000000000000000006666666666666666666666601111111111111111111111111111111666666666666666666666666600000000000000000000000
00010000000000000000000000666666666666666666666601999999111111999991111119999991666666666666666666666666000000000000000000000000
00000000000000000000000000066666666666666666660011199999911119999999111199999911006666666666666666666660000000000000000000000000
01000000000000000000000000006666666666666666001110199989999999992999999999899911110066666666666666666600000000000000000000000010
00000000000000000000000000000066666666666660111111199889988899892989988899889911111106666666666666660000000000000000000000000000
00100000000000000000000000000000666666666601111111199898888889892989888888989911111110666666666666000000000000000000000000000000
01000000000000000000000000000000006666666011111111199898899889892989889988989911111111066666666600000000000000000000000000000010
01000000000000000000000000000000000066666011111111999889988889892989888899889991111111066666660000000000000000000000000000000010
00100000000000000000000000000000000000660111111111999888889998892988999888889991111111106666000000000000000000000000000000000000
11010000000000000000000000000000000000000111111111999889998888892988888999889991111111106600000000000000000000000000000000000011
00010000000000000000000000000000000000000011111111999898888888892988888888989991111111110000000000000000000000000000000000000000
00000000000000000000000000000000000000000000111111999898888888892988888888989991111111000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000001111199889888888892988888889889911111100000000000000000000000000000000000000000010
10000000000000000000000000000000000000000000000011199888988888892988888898889911110000000000000000000000000000000000000000000001
00110000000000000000000000000000000000000000000001199888988888892988888898889911100000000000000000000000000000000000000000000000
01010000000000000000000000000000000000000000000000199889888888892988888889889911000000000000000000000000000000000000000000000010
00010000000000000000000000000000000000000000000000199898888888892988888888989910000000000000000000000000000000000000000000000000
00010000000000000000000000000000000000000000000000019898888888892988888888989110000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000019898888889892989888888989100000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000019898888899892989988888989100000000000000000000000000000000000000000000000010
00000000000000000000000000000000000000000000000000199898888898892988988888989910000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000000000000000199898888899892989988888989910000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000199898888882892982888888989910000000000000000000000000000000000000000000000010
01000000000000000000000000000000000000000000000000199898888888892988888888989910000000000000000000000000000000000000000000000010
00100000000000000000000000000000000000000000000001199898888888892988888888989910000000000000000000000000000000000000000000000000
11010000000000000000000000000000000000000000000001999898888888892988888888989991000000000000000000000000000000000000000000000011
00010000000000000000000000000000000000000000000001999889888888892988888889889991000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000001999888999998892988999998889991000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000001999888888889892989888888889991000000000000000000000000000000000000000000000010
10000000000000000000000000000000000000000000000001999888888888892988888888889991000000000000000000000000000000000000000000000001
00110000000000000000000000000000000000000000000001999999999999992999999999999991000000000000000000000000000000000000000000000000
01010000000000000000000000000000000000000000000001111111111111111111111111111111000000000000000000000000000000000000000000000010
00010000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000000000000000000000
0001000000000000000000000000000000000000000011c100000000000000000000000000000000000000000000000011100000000000000000000000000000
00000000000000000000000000111100000000000011cccc100000000000000000000000000000000110000000000001ccc10000000000000000000000000000
01000000000000000000000001cccc100000000001ccccccc10000000000000000000000000000001cc100000000011ccccc1000000000000000000000000010
0000000000000000000000001cccccc10000000000111cc110000000000000000000000000111111cccc100000011cccccccc100000000000000000000000000
001000000000000000000001cccccccc110000000000011100000000000001100000000011cccccc1cccc100011ccccccccccc10000000000000000000000000
01000000000000000000001ccccccccccc11000000001c100000000000011cc100000011ccccccccc11110111cccccccccccccc1110000000000000000000010
0100000000000000000001cccccccccccccc11000000010000000000001ccccc100001ccc11ccccccc1001cccccccccccccccccccc1110000000000000000010
001000000000000000011ccccccccccccccccc11000000000111000001ccccccc1001cccccccccccccc11cccccccccccccccccc11cccc1000000000000000000
1101000000000000001cc11cccccccccccccccc1000000011ccc1100001111111000011111ccccc11110011ccccccccccccc111ccccc10000000000000000011
000100000000000001ccccc111cccccccccccc100000011ccccccc1000001c100000000000111110000000011111ccccccccccccccc100000000000000000000
0000000000000000001ccccccccccccccc11110000001cccccccccc110000110000000000001c100000000000000111ccccccc11111000000000000000000000
0110000000000000000111ccccccccc1110000000001ccccccccccccc1000000000000000001c100000000000000000111111100000000000000000000000010
100000000000000000000011111111100000000000001111cccccc1110000000000000000001c1000000000000000001c1000000000000000000000000000001
00110000000000000000000000001c100000000000000000111111000000000000000000001c10000000000000000001c1000000000000000000000000000000
01010000000000000000000000001c1000000000000000001c1000000000000000000000001c10000000000000000001c1000000000000000000000000000010
00010000000000000000000000001cc1000000000000000001c100000000000000000000000000000000000000000001c1000000000000000000000000000000
000100000000000000000000000011c1000000000000000001c100000000000000000000000000000000000000000001c1000000000000000000000000000000
000000000000000000000000000001c100000000000000001c100000000000000000000000000000000000000000001c10000000000000000000000000000000
010000000000000000000000000001c1000000000000000001000000000000000000000000000000000000000000001c10000000000000000000000000000010
000000000000000000000000000001c10000000000000000000000000000000000000000000000000000000000000001c1000000000000000000000000000000
00010000000000000000000000001c100000000000000000000000000000000000000000000000000000000000000001c1000000000000000000000000000000
1101000000000000000000000001cc10000000000000000000000000000000000000000000000000000000000000000011000000000000000000000000000011
00100000000000000000000000001100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
101c001000000110001111000110000000111100011000000011110001100000001111000110000000111100011000000011110001100000011000000100c101
10c60001010010001000010100010010100001010001001010000101000100101000010100010010100001010001001010000101000100100001001010006c01
10000101001101000000100100101100000010010010110000001001001011000000100100101100000010010010110000001001001011000010110010100001
11111000000001000000001000100000000000100010000000000010001000000000001000100000000000100010000000000010001000000010000000011111

__map__
00001c1dc1131c1d1a1a1a1a1c1d130505d0d11c1d1402064a49021500000000000000000000001c1d001c1d13000000000000000000000000000000041c1d3e470400141302020200130000000000000000000000000000000000d0d1c0c113d0d114000000000000000000000000c10202020114410000001c1d0000000000
1c1d2c2d02022c2d021c1d1a2c2d0202021c1d2c13022b0c0c0c131400000010010102c11300002c2d002c2d020000040000d0d104000000000000c1022c2d40020202272521c00c002a2525252100000000000000000000000000020202c0020202020000000000000000000000c00202d0d1014a470000062c2d0101000000
2c2d100202020202022c2d0202020202042c2d04021c1d0c0c0c0202000000022b0202020200000101010101020000022b021c1d0202000000001c1d02020240022b02241a15c10c0002010101020000001c1d00464c45301a1a0002c002d0d1021515000000000000000000001c1d0202c133353a010010020202021c1d0000
02020202010101021302020202060002021c1d02022c2dd0d1c00101000000000101011c1d000202c0d0d1c10200000202022c2d0c02000000002c2d0244423f020205293a02140c0002010101020000002c2d02023e47341a1a0002272101c1021515000000000000000000002c2d1c1d1c1dc0150400d0d1021c1d2c2d0000
00020202010101020202021c1d020002102c2d010100000000000000000000000101012c2d000202021c1d02020000424242424502020000000044424247c046424505022b41020200020202020202000013c10202402b3a1a1a00c124c001c12bc1140000000000000000001c1dc02c2d2c2d1415150000c0022c2d13000000
0000d0d10101010002021a2c2d0c0000000000000000000000000000001302c1000000000000d0d1022c2d021c1d0002131c1d400505000000004013021c1d022b4002024a471c1d001c1d02d0d102000002444b42472b021c1d00c02a0201020202020000000000000000002c2d300c0c2b0c02020300000202020202000000
000000000000000000000000000000000000000000000000000000000006021c1d0000000000022b1a1a1a2b2c2d0002022c2d400505000000004002022c2d020240020006022c2d002c2d020202020000023e3f2b0202022c2d0000000000000000000013020c010102020002023a0c0c0c0c02020200000000000000000000
0000000004000000000000000000001c1d03d0d10000000000000000000c0c2c2d00000000000200001c1d02c100000000000000000000000000000000000000000000000000001c1d00d0d11300000000000000000000001400000000000000000000000202010101020200d0d10c0c0c0cc102c00400000000001c1d02ae00
000000000505051c1d0000000000002c2d020610000000200141030600c0012b0200000000000200002c2dc10000000000000000000000000000000000000000001c1d000000002c2dc0140202000000000000000000020202020502021c1d000000000000000000000000000000000000003a31050500000000002c2d021c1d
0000000002c1022c2d0c0c0c0c0c0000020202020200002a2b400601000101020c0000000000000000000000001c1d0000000000000000005a55510402050505032c2d000000151402020c02020000000c020104c0140002020202132a2c2d0000000000000000afbeafa3af000000000000000000000000000000c102022c2d
00001c1d02c002c10202444b450200000c020101c100000202464901000000040206060c0202232a00000000142c2d00030003c1401c1d001c1d02020205050502c114410c021515020202c1020000c14449010102010002d0d102232815021400afa3000000009f92afa2a200000000000000ae92a2a2a292a20000921c1d00
00002c2d022b0202444b43434702000000000000000000000000001300000002100202c00204020200000000013ac1020206020c402c2d002c2d1c1d0205050502272146490202021c1d2b0202000403480202050101000202021c1dc0022b02009e02afbe0000afa2afa2a2000000000000000000000000009d00009c2c2d00
00002a2521c002444c3fd0d1040200130014050505020202141c1d0200000015c10202020c02051400001c1d3338152b0202020240025a0002022c2d2b04c114c02a0200000202022c2d020202000c021402021c1dc11c1dc1022c2d040202060000a292020000a2a2a292a20000000000000000000000000000000000000000
000020c1022b023e434713c1020200020c0c4a4249d0d102022c2d02000000c10c0c0cc1021c1d0c00002c2d021c1d02020c0c0c4002540002020202020202020202c000000000000000000000002b0201d0d12c2d002c2d0c0c0c0c0cd0d10400009d0000000000000000000000000000000000000000000000000000000000
000029252ac102400c0c02020200000204c12725210204020202001a0000001402c00202132c2d0200000014022c2dc0020202444702520002d0d115c1444242451c1d0000000000000000000000060201150000000000000c4a42490c020c02009f92a2a200000000000000009e029200000000000000000000000000000000
00001c1dc00202400c1c1dd0d10000020215241c1d0102060202001a1a00000202d0d1020202232a000001020202022002022b40020202000202151544470606402c2d00001e1e1e1e000500ae1c1d0400000000000000000c0c0c0c0c000000000000af9d00000014d0d1000000000c00000000000000000000000000000000
00002c2d060644470c2c2d1c1d0000022b15242c2d2a4102021c1d0000000000000000000000000000000000000000292a000000000000000015151540020606464242001e1e4c4c4c1e3a02922c2d021c1d0000000000000000000000000000000000000000009ea2a215150000005000000000000000000000000000000000
00000000020240020202022c2d00000202c0292525284802022c2d0000020220c0020202000014040000000000000000000000000000000000000000000000000006001e1e4c4c4c4c4c32b9020202022c2d000000000000000000000000000000000000000000d0d1a215c10000005aa2ae0000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000050624020202022b020202000000001c1d04000003000000000300001400464c3f04000000001e1c1d4c4c4c4c4c4c4c1c1db8c00400000000000000000000000000000000000000000000c1a2c0140000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000005c124020202021c1d0101000000002c2d0c02020200000000020202021c1d3e3f202a0000001e2c2d4c4c4c4c4c4c4c2c2db7029200000000000000000000000000000000000000000000c092c1a29c000000009c0202ae00001c1d00000000d0d1ae
000000000000000000000000000000000000000000000000000000000005022ac10202022c2dc001000000004a4242491c1dc01400000202d0d12c2d463f24240000001e00020c4c4c4c4c4c4c4c020202990000000000000000000000000000000000001c1d0000c19dd0d100000000001c1d1a0000002c2d00000000020202
00000000000000000000000000000000000000000000000000000000000202c01515c0041c1d01010000002a2521060c2c2d04010000050202020202154029280000001e0002024c4c4c4cb9020202021c1d0000000000000000000000000000000000c02c2d000000ae1c1d00000000002c2d001c1d0c000000000000021c1d
000000000000000000000000000000000000000000000000000000000014c11515c102022c2d02130000000002c114022b150101000004c01c1d2b444b3f14c10000001e1e0202024c4c02020202a9022c2d000000000000000000000000000000000202021e000000922c2d00000000000001002c2d1c1d0000000000022c2d
0000000000000000000000000000000000000000000000000000000000020202c0020101000002020000000002050202c00202000000202a2c2d023e434702020000001c1d0202024c02021c1d535a029c00000000000000000000000000000000001c1d02ae001c1d02d0d19b00000000001c1d00002c2d0000000000020202
60000000000000000000000000000000000000000000610000000000000000000000000000000000000000000000000000000000000029281c1d024002022b020000002c2d0202020202992c2db90202000000000000000000000000000000001c1d2c2d1e1e002c2d0202aaabacad0000002c2da2a2000000000000001c1d02
1eec000000000000000000000000000000000000007a1e1e000000000000000000000000000000000000000000000000000000000000d0d12c2d004645c00200000000c10202a9020202020202000202100000000000000000000000001c1d002c2d1e1c1d1e00001c1d02babbbcbd00000000a2a2a2000000000000002c2d02
1e1eec00000000000000000000000000000000007a1e1e1e000000000000637272620000000000000000000000000016002b00000000000000000000000000000000000000000000000000000000b0b1b2b300000000000000000000002c2d1e1e1e1a2c2d1e00002c2d02a3bfaf1c1d0000af9dafa3af000000000000020202
1e1e1ee7e6000000000000000000000000004f4e1e1e1e1e0000000000004d7979e90000000000000000000000000015142a000000000000000000000000000000000000000000000000000000005c5d5e5f00000000000000001c1dc01e1e1c1d1e1e1e1e1e001c1d02020202022c2d00000000000000000000000000c19cc0
1e1e1e1e1e600000000000000000000000611e1e1e1e1e1e0000000000617479767560000000000000000000001011001329000000000000000000000000000000000000000000000000000000006c6d6e6f00000000000000002c2d1c1d1e2c2d1e1c1d1e1e002c2d94951002d0d10000000000000000000000009495001000
1e1e1e1e1e1eea0000000000000000007b1e1e1e1e1e1e1e000000007b797979797979ea0000000000000000000f00120028000000000000000000000000000000000000000000000000000000007c7d7e7f00000000000000001e1e2c2d0c001e1e2c2d1e1e000000a4a502020000000000000000000000000000a4a5ae929d
1e1e1e1e1e1ee90000000000000000004d1e1e1e1e1e1e1e000000004d797973797979e9000000000000000000000e000000270000000000e0d2d2d2d2d2d2d2d2d2d2d2d2e100000000c4c5c6c7c8c9cacbcccdcecf00000000000000000000000000000000000000000000000000000000000000000000000000029c009c00
1e1e1e1e1e1e1eea000000000000007b1e1e1e1e1e1e1e1e0000006a79797979737979796b000000000000000b0c0d000000260000000000c2000000000000000000000000c300000000d4d5d6d7d8d9dadbdcdddedf000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
010200001c7250550103501005150170000600107150270501705017050170501705007050070507705067050670306703067030570304703037030270301703007030570304703047030c0030c0030c00300000
0001000004130041300413004120121201212012120121200412004120041200412017120171201712017120041200412004120041201c1201c1201c1201c1200412004120041200412021120211202112021120
010200003c611246150c5240c52530611186151a5241a525246110c6152f5142f515186110c615000050000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000300000d1450c0250c55509705097050970509705097000d7001f7001d7001b7001a700187001670015700147001270011700107000d7000c7000a700097000870007700077010670100000000000000000000
000200001c1151e53516045130550f0550d7550a755087550875510755157451a0301402000710007000070000700007000070000700007000070000700007000070000700007000070000700000000000000000
0003000008610046100b610071100e62027110146102c0101b610010100001022510005101b52024520245101b510010100001000000000000000000000000000000000000000000000000000000000000000000
0110000018944199541a9441b9541c9441d9541e9641f964209642197422974239741a8001c9001a900189001c900189001a9001c1001c1001c1001c100041000410004100041002110021100211002110000000
000400000d74500154105451a0001f7001b0001a7001d000170000370001700007000d700087000c700290000e700000000000000000000000000000000000000000000000000000000000000000000000000000
0001000000000107101772020720287101371017700247002c7002570023700000003770037700377001970019700197000000000000000000000000000000000000000000000000000000000000000000000000
000200000f750251102112036520231201c12016120345202013017130121203352023110141100d1101451000700142000000000000110000000000000000000000000000000000000011000000000000000000
00010000031100411005110081100a1200c1300c1400b1400814007130071300713007130091200a1200d12012120190301c0300b0300d0400f0400f04000610006100c060030500304001140011000210002100
00020000291001d100006102b0201974003710087000e7001f7051754500605015052453007701057020d7002f520301002900016000160001a000290000b3001050413501355052270522705225042e5053b505
000100001d620056200462004620036200362003620011203d0202c3200562005620021203e0202a3200562001120011203c0202c3200862008610011103a0102c31005610056103251031510035101250037500
0002000008610046200b620221200e63019120146200f0201b620006200002014520005200b530065300552004520000200001000000000000000000000000000000000000000000000000000000000000000000
00020000030500501006040100302361019620126300d630076000861008610076200563004630046300462004620046200560005610056100561005610056100561005610006000061000610006100061009600
000200002d110345102311031510271102c520126300d6302b5302811024510076200563004630046300462014010100000d010056100561005610056100b0200800005610006000061000610006100061009600
000200200013400100001350015300175000000013400135001340003500105000340013000150000700006500154000500005500105000530003300130001350013400151000510013500000000350015400155
000200200053403555035740355500535000000053400555000740006500055000000053400555005450000000034000550004418025000000054400555005740017300000005540057100555000000005300000
00020000177102d0100d0103271025510181101f5101b1103351017110215101d11132511231102a5101d1102b5102511010110387101d7100271033700377003d70033700367002370027700077000b70000000
00040000165751e7750a5740757513774045750257406775067750157501574015750b77400575005640576500164007650275400555000540054500544017350052500515015040150501504015050150401500
000200000074400141007410010100051000010015100101000410000100741001310073500720001150b6050d6000f600106001160011600116001160011600106000f6000b6000a60003000050000300007000
000100000b3000560003510046000360005510057100151037010295100560003510067102a0101e51005600005100701038010297100860031010225100d5100351005600310202351004510035101250037500
0002000000135000533571527533001453071331015007530014327515017530475300035287132d71500113006050c003006050c603006050c003006050c6030c603006050c0030c603006050c003006050c003
01300000000000000004000040000100000000070050c00500005000050c0050c00501005000050c0050c005000050000504005040050100500005070050c0050000500005000030100505000051001050011500
01300000050050500500002010020500505005145051150510505130050c00500000240051c50518005110050d0050d0050f005180050c0051600514004130021000510005185051800510505100052450530005
011c00000155501515050000100004555045151d0051d0050155501521005112401507000180151a0000c015075152e0150000000000140001350513000125051200015005110000000000000000000000000000
010e0000005533d1053d1050000200000000000055305005005530000007000145050055310505005530c005005532400500000000000500018005040000d005005530f0050c0000d0000e000130000055310005
011800000010000155001550c0050c00100155000000c0000010000155001550c0050000000155000000c0000000000155001550c0050c00100155000000c0000010000155001550c0050c001001550000000000
011600001100000000150000c60518000000001500027500110000000014000270001800000605110000c6001000000503130001860518000000001c000000001f0000c600190000000015000006001000000000
011800001800524001240052400524005000002f705307053100525005190050d00530000300050c00500000300052400518005005053b7052f705237050b5052e00522005160050a00507504135011f5011f505
0118000002050020450205002035065500654506050060350305003045020500203509550095450e0500e0350205002045020500203502150021410215502115030500304502050020350e1200e1210e12502115
0118000002050020450205002035065500654506050060350305003045020500203509550095450e0500e0350205002045020500203503550035450e0500e0450505005035125501255503050030251155211532
0118000002050020450205002035065500654506050060350305003045020500203509550095450e0500e035020500204502050020450202300053030550200007050070450715007145125351f5350653513555
011800000f0650f0150f0400f03511045110151a0401a0350e0400e035180201802516044160351504215032120401203512040120351a5401a5351a0401a0351254012535120401203526530265353201032015
01180000265002550026500265001a5000e5000e5000e5000e5000e5000e5000d5000c5000b5000a5010950109502095020950209502095020950209502095020850208502085010850500000000000000000000
011800001050010500105001050210502105021050210501045010450204502045050000000000000000000015504155021550215502155021550215502155010950109502095020950509505000000000000000
011800000f0400f0350f0400f03511040110351a0401a0350e0400e035180201802516044160351504215032120401203512040120351a5401a53502000000001254012535210302102524520245151612016125
01300000050550505500062010420506505055145351155510555130350c05500000240351c52518055110250d0550d0550f055180550c05516015140541305210055100551855500000105551f0452253514125
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01300000000000000004000040000100000005070050c00500005000050c0050c00501005000050c0050c005000050000504005040050100500005070050c0050000500005015050000503005105050100503505
01300000000550005504055040550105500055070550c05500055000550c0550c05501055000550c0550c055000550005504055040550105500055070550c0550005500055015550005503055105550105503555
01300000006150c013006150c613006150c013006150c6130c613006150c0130c613006150c013006150c013006150c013006150c613006150c013006150c6130c613006150c0130c613006150c013006150c013
01300000000550005504055040550105500055070550c05500055000550c0550c05501055000550c0550c055000550005504055040550105500055070550c0550005500055000530105505055051551053611556
01300000050550505500062010420506505055145351155510555130350c05500000240351c52518055110250d0550d0550f055180550c0551601514054130521005510055185551805510555100552454530015
0118000007050070550705007055020600206103041030450706007065070500705516545165151354513515125501254515030150350e0300e041020410204526030260351e5201e5251a0501a0551302013025
01300000050550505500062010420506505055145351155510555130350c05500000240351c52518055110250d0550d0550f055180550c05516015140541305210055100551855500000105551f0452253514125
0118000000050000110c0550c0310c015000000c0500c0110105001011000550003100015000000c00500000130651205511545100550f53500000075550000008535030050b545050050e04509005135650e005
01180000186150c6150c0233f215006110061100615000000c61118615005130011318615000000c613000000c6100c61100615000000c01300611006110061500615000000c0330c12300615000000c01300615
011800001803524011240552403524015000002f725307153102525025190350d03530000300050c00500000300252402518035005353b7252f725237350b5352e01522025160250a03507544135411f5211f515
011800000e5450e5150e5340e5310e5210e5150e5340e5310e5210e5150f5450f51515535155151a5351a5150e5450e515211142151121111215152c7142b7152452524515225252251521525215150000000000
011800000e5440e5250e5340e5310e5210e5150e5340e5310e5210e5150f5440f51515534155151a5341a5151b5441b5251a5341a5311a5211a51515534155351353515534155351353512534125350000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 0a0b4344
00 43444344
00 47094344
00 48494344
01 4c424344
01 595b4344
02 5a424344
01 50424344
02 51424344
00 53544344
01 292a7244
00 2b2a7344
00 2e2a4344
00 2c2a4344
00 2f307444
00 31304344
00 2f304344
00 31304344
00 1e307344
00 1f304344
00 1e304344
00 20304344
00 2d304344
00 24304344
00 2d304344
00 21304344
00 30424344
02 30424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 01424344

