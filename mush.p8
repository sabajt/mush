pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
-- mushroom mountain
-- by jsaba

function _init()
    printh("", "mush", true)
    cartdata"jsaba_mushroom_mountain"

    local f=tb"0,0,0,0,0,0,1,1,0,0,0,0,2,2,1,1,0,0,3,3,3,1,1,0,4,4,2,2,1,0,5,5,1,1,0,0,6,5,5,1,1,0,7,6,6,5,5,0,8,13,2,1,1,0,9,4,4,2,1,0,10,9,9,4,4,0,11,3,1,1,0,0,12,13,13,1,1,0,13,13,2,2,0,0,14,2,2,13,5,0,15,9,4,4,1,0,"
    fades={}
    local fi,ftab=0
    for c in all(f) do
        if fi%6==0 or fi==#f-1 then 
            if (ftab) add(fades, ftab)
            ftab={}
        end
        add(ftab, f[fi+1])
        fi+=1
    end

    puzzles,opdirs,bodylut,dissolve_particles,stepdur_org,stepdur,clouds_x,clouds_y,clouds_y_org,mountain_y,mountain_y_org,door_y,door_y_org,tmlx,tmlx_org,tmrx,tmrx_org,shovtarg,transition_clock = {},tb"u=d,d=u,l=r,r=l,",tb"lr=37,rl=37,rr=37,ll=37,ud=36,du=36,uu=36,dd=36,ul=40,lu=40,ur=41,ru=41,dl=38,ld=38,dr=39,rd=39,",{},6,7,0,48,48,46,46,60,60,4,4,38,38,vec(112, 7),clock(.66)
    puzzledata = "dev_0_13_8_6|west trailhead_0_0_11_7_to use this trail,_please plant mushrooms_on plots of dirt_by pressing \151.|volunteer path_12_0_7_6_this path is maintained_by volunteers like you._\"leave only mushrooms,_take only memories\"|foothills rune site_20_0_8_7_historical rune site,_established 1xxx._enjoy using these_special doors._you'll definitely_end up somewhere!|central foothills_29_0_8_5_the abundance of mushrooms_on the mountain_makes time quite squishy._press \142 to backtrack.|switchback connector_38_0_8_9|east face rune site_46_0_12_8|east river fork_59_0_10_12|crawler's sanctuary_70_0_9_7_do not feed the wildlife._it may eat you \136 \136|central bluff_80_0_10_8|volunteer rock garden_91_0_8_7_crawler factoids:_1.they \135 moving ahead_2.kinda like turning right_3.don't like turning left_4.hate turning around,_but will if they must.|sacred ground_100_3_8_5|west overlook_109_0_7_8|secret corner_100_0_8_3_help maintain the trail._dig up grass to make_new plots of dirt_by pressing \151._to keep the trail_beautiful for everyone,_all shovels will_dissolve after 1 use.|kill the snake_29_8_6_5|spare the snake_29_17_11_7|"
    local curpuz,buffer,cursigns,i = {},"",{},0
    for j=1,#puzzledata do
        local c = sub(puzzledata,j,j)
        if c == "_" then
            if i >= 5 then 
                add(cursigns, buffer)
            elseif i >= 1 then
                add(curpuz, tonum(buffer))
            else
                add(curpuz, buffer)
            end
            i += 1
            buffer = ""
        elseif c == "|" then
            if i >= 5 then
                add(cursigns, buffer)
                add(curpuz, cursigns)
                cursigns = {}
            else 
                add(curpuz, tonum(buffer))
            end
            add(puzzles, curpuz)
            curpuz,buffer,i = {},"",0
        else 
            buffer = buffer..c
        end
    end
    setpuzzle(1)
    gt,mapnodes = clock(1),{}
    for x=40,55 do
        for y=27,36 do
            local s = mget(x, y) 
            if (s > 0 and s ~= 30 and s < 64) add(mapnodes, {p=vec((x-40)*8, (y-27)*8+12),s=s})
        end
    end
    music(10, 0, 3)
    init_title()
end

function tb(s)
    local r,i,buf,lhs={},1,""
    for i=1,#s do
        local a=sub(s,i,i)
        if a=="=" then
            lhs,buf=buf,""
        elseif a=="," then
            local b = tonum(buf) or buf
            if lhs then
                r[lhs],lhs=b
            else
                add(r,b)
            end
            buf = ""
        else
            buf=buf..a
        end
    end
    return r
end

function setpuzzle(i)
    pz = puzzles[i]
    pz_i,pz_name,pz_x,pz_y,pz_w,pz_h = i,pz[1],pz[2],pz[3],pz[4],pz[5]
end

function init_nextpuzzle()
    setpuzzle(pz_i+1)
    init_heading()
end

function loadgame()
    if dget(0) >= 0 then
        setpuzzle(dget(0)) 
        init_heading()
    end
end

function savegame()
    dset(0, pz_i)
end

function init_title()
    menuitem(1)
    menuitem(1, "load game", loadgame)
    state,hidemain,from_title,transition_state = "title",false,true,"in"
end

function init_load()
    state,transition_state = "load","in"
end

function init_heading()
    playsfx(-2, 2)
    playsfx(-2, 3)
    menuitem(1)
    menuitem(1, "save game", savegame)
    menuitem(2, "load game", loadgame)
    state,hidemain,transition_state,transition_animate = "heading",false,"in",transition_animate and update_heading_transition
end

function init_puzzle()
    menuitem(1)
    menuitem(1, "restart puzzle", reset_puzzle)
    menuitem(2, "save game", savegame)
    menuitem(3, "load game", loadgame)
    state,won,forcefade,died,die_time,steptime,undo,hud,willadd_shrooms,willdel_shrooms,shovels,changedirt,transition_state,willmoveto,willblock,outsnake_local,outblock = "puzzle",false,false,false,0,0,false,tb"col=1,",{},{},0,0,"in"
    loadmap()
end

function _update()
    sfxc = {}
    if (transition_state) update_transition()
    local cx,go = wrap(clouds_x-.16, -128, 0),btnp(4) or btnp(5) and not transition_state
    if state == "puzzle" then
        if windelay then 
            clocktick(windelay)
            if (windelay.perc == 1) transition_state,transition_next,forcefade,hidemain,windelay = "out",init_nextpuzzle,true,true,nil
        end
        update_puzzle()
    elseif state == "heading" then
        clouds_x = cx
        if go then
            playsfx(2, 3)
            from_title,transition_state,transition_animate,transition_next = false,"out",nil,init_puzzle
        end
    elseif state == "title" then
        clouds_x = cx
        if go then
            playsfx(16, 2)
            playsfx(17, 3)
            transition_state,transition_next,transition_animate = "out",init_heading,update_title_transition
        end
    end
    update_lastdowns()
    clocktick(gt)
end

function update_title_transition()
    local t = transition_clock.perc
    door_y,clouds_y,mountain_y,tmlx,tmrx = lerp(door_y_org, door_y_org+(128-door_y_org)/2, t, sinein),lerp(clouds_y_org, clouds_y_org+3, t, sinein),lerp(mountain_y_org, mountain_y_org-3, t, sinein),lerp(tmlx_org, tmlx_org-48, t, sinein), lerp(tmrx_org, tmrx_org+48, t, sinein)
end

function update_heading_transition()
    local t = transition_clock.perc
    door_y,clouds_y,mountain_y,tmlx,tmrx = lerp(door_y_org+(128-door_y_org)/2, 128, t, sineout),lerp(clouds_y_org+3, clouds_y_org+6, t, sineout),lerp(mountain_y_org-3, mountain_y_org-6, t, sineout),lerp(tmlx_org-48, tmlx_org-96, t, sineout),lerp(tmrx_org+48, tmrx_org+96, t, sineout)
end

function dissolve_cluster(pos, col, parts, rad, dur)
    for i=1,parts do
        local v = pvec(rnd(8)-4, rnd(1))
        local p = pos + v + vec(4,4)
        add(dissolve_particles, {
            col=col,
            startpos=p, 
            endpos=p + pvec(rnd(4) + rad, rnd(1)),
            clock=clock(rnd(.4) + (dur or .3))
        })
    end
end

function snake_dissolve(snake, killtype)
    local c,d = 8,.33
    if killtype == "poison" then
        c,d = 7,1.5
    end
    dissolve_cluster(cellpos(snake.cell), c, 5, 2, d)
    for b in all(snake.body) do
        dissolve_cluster(b.pos, c, 5, 2, d)
    end    
end

function player_dissolve_command(cell, dur, doflash, fromsnake)
    local c = fade_command(pl)
    local s = c.start
    c.start = function()
        s()
        playsfx(fromsnake and 9 or 19, 3)
        for col in all(tb"7,13,2,") do
            dissolve_cluster(cellpos(cell), col, 8, 4, dur)
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
            playsfx(item == pl and 15 or 14, 3)
            dissolve_cluster(cellpos(cell), 12, 12, 1)
        end
    end
    return c
end

function draw_dissolve()
    for p in all(dissolve_particles) do
        local t = p.clock.perc
        local pos = lerpvec(p.startpos, p.endpos, t, sineout)
        pset(pos.x, pos.y, lerptab(fades[p.col+1], t))
    end
end

function update_puzzle()
    if signtext then 
        sign_t = min(sign_t + (sign_hiding and -.12 or .12), 1)
        if sign_t == 1 and btnd() then 
            pl.pos = pl_lastpos
            if sign_i+1 < #sign_texts then
                sign_i += 2
                signtext = sign_texts[sign_i]
                signtext_2 = sign_texts[sign_i+1]
                signto = signtarget(signtext)
            else
                sign_hiding = true
            end
            playsfx(7, 2)
        end
        signpos = lerpvec(signpost_pos, signto, sign_t)
        if sign_hiding then
            if (sign_t <= 0) signtext, sign_hiding = nil
        else
            local x, y = 0, 0
            if (nextmovedir == "r") x += 3
            if (nextmovedir == "l") x -= 3
            if (nextmovedir == "u") y -= 3
            if (nextmovedir == "d") y += 3
            pl.pos = lerpvec(pl_lastpos, pl_lastpos + vec(x, y), sin(-min(sign_t*2,1)/2))
            sign.pos = lerpvec(signpost_pos, signpost_pos + vec(x, y), sin(max(sign_t*2,1)/2)) 
        end
        return
    end

    if not won then
        update_pz_input()

        --update puzzle commands
        if steptime > 0 then
            if steptime == stepdur then 
                if (undo) playsfx(0, 2)
                placehud_pos = nil
            end
            growmush_sfx_played = false
            local begin = steptime == stepdur
            steptime -= 1
            local t = stepperc()

            for stack in all(commands()) do
                if begin then
                    stack[#stack].start()
                end
                -- update command
                if #stack > 0 then
                    local c = stack[#stack]
                    c.run(t)
                    if t == 1 then
                        c.next()
                        if undo then
                            del(stack, c)
                        end
                    end
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

    -- update dissolve
    for p in all(dissolve_particles) do
        clocktick(p.clock)
        if (p.clock.perc == 1) del(dissolve_particles, p)
    end

    die_time = max(die_time - 1, 0)
    if die_time == 1 then
        die_time = 0
        transition_state = "out"
        transition_next = init_puzzle
    end
end

function canpushto(to, dir, thrudoor)
    local block = blocktab.item(to)
    if (block) return canpushto(stepcell(to, dir), dir, thrudoor)

    local seg = is_snake(to)
    local snake = seg and seg.snake
    local istail = snake and snake.body[#snake.body] == seg

    if inbounds(to) and 
      (#items(to) > 0 or watertab.item(to)) and 
      (not seg or istail) and 
      not shoveltab.item(to) then

        local indoor, outdoor = doorpair(to)
        if outdoor then
            if outdoor.open then
                local landing_cell = stepcell(outdoor.cell, "d")
                return canpushto(landing_cell, "d", true)
            else
                return
            end
        elseif indoor == goaldoor then
            return
        end
        return to, thrudoor
    end
end

function add_push_block_commands(batch, lastpushto, movedir)
    local curcell = stepcell(lastpushto, opdirs[movedir])

    local _, outdoor = doorpair(curcell)
    if outdoor then
        curcell = stepcell(outdoor.cell, "d")
        movedir = opdirs[movedir]
    end

    while blocktab.item(curcell) do
        local pushcell = stepcell(curcell, movedir)
        add(batch, movepushblock_command(blocktab.item(curcell), pushcell))
        curcell = stepcell(curcell, opdirs[movedir])

        local _, outdoor = doorpair(curcell)
        if outdoor then
            curcell = stepcell(outdoor.cell, "d")
            movedir = "u"
        end
    end
end

function update_pz_input()
    if (btnu(2)) invertcontrols = false
    if steptime == 0 then
        undo = (not died and btn(4) and #pl.commands > 0)
        stepdur = undo and flr(stepdur_org*.85) or stepdur_org
        local nextcell

        if not undo then
            local pl_cell = pl.cell
            local maybenext = pollnext(pl_cell)
            local _, outdoor = doorpair(maybenext)
            local nextdoor = doortab.item(maybenext)
            local dooropen = nextdoor and nextdoor.open
            nextmovedir = dircell(pl_cell, maybenext or pl_cell)
            nextcell = (dooropen and outdoor) and stepcell(outdoor.cell, "d") or maybenext
            local facecell = stepcell(pl_cell, pl_dir)
            local nextmoved = pl_cell ~= nextcell
            local facemush = mushtab.item(facecell) 
            local nextmush = mushtab.item(nextcell)
            local nextshovel = shoveltab.item(nextcell)

            if nextmoved and signtab.item(nextcell) then
                signto, signtext, signtext_2, sign_t, sign_i = signtarget(sign_texts[1]), sign_texts[1], sign_texts[2], 0, 1
                pl_lastpos = pl.pos
                playsfx(11, 3)
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

            local batch = {}

            -- player moved
            if not transition_state and
                not died and 
                inbounds(nextcell) and 
                #items(nextcell) > 0 and 
                nextmoved and 
                ((not nextdoor) or (dooropen and nextmovedir == "u")) and 
                ((not pushblock) or pushto or pushblock.inwater) then -- trigger step:

                steptime = stepdur
                willmoveto = nextcell
                local move = moveplayer_command(pl_cell, nextcell)
                local enterdoor = nextdoor and nextdoor.open or false
                add(batch, move)

                if enterdoor then
                    if nextdoor.isgoal then 
                        pl_mask = false
                        add(batch, fade_command(pl))
                        add(batch, win_command(nextdoor))
                    else
                        batch = {thrudoor_command(maybenext)} 
                        if pushto then
                            add_push_block_commands(batch, pushto, pushthru and "d" or "u")
                        end
                    end
                elseif pushto then
                    add_push_block_commands(batch, pushto, pushthru and "d" or nextmovedir) 
                else
                    if nextmush then
                        steptime = stepdur
                        if nextmush.type == 26 then
                            add(batch, delmush_command(nextmush, true)) -- sfx pick up
                        else
                            add(batch, player_dissolve_command(nextcell, 1.5))
                            add(batch, die_command())
                            add(batch, delmush_command(nextmush)) 
                        end
                    elseif watertab.item(maybenext) and not waterblocktab.item(maybenext) then
                        add(batch, splash_command(pl, nextcell))
                        add(batch, die_command())
                    end
                end

                if (nextshovel) add(batch, getshovel_command(nextshovel))
            end

            -- actions
            if stepdur ~= steptime and not died and btnd(5) then
                local attackseg = canattack(facecell)
                if can_place_mush(facecell) then
                    steptime = stepdur
                    add(batch, placemush_command(facecell))
                elseif digtype(facecell) then
                    steptime = stepdur
                    add(batch, dig_command(facecell))
                elseif attackseg then
                    steptime = stepdur
                    add(batch, killsnake_command(attackseg.snake, "shovel"))
                end
            end

            -- predict snake
            local seg = is_snake(nextcell or pl_cell, true)  
            if seg and not seg.snake.willdie and not died then
                add(batch, player_dissolve_command(nextcell, .6, true, true))
                add(batch, die_command())
            end

            -- auto-advance if died
            if (died and steptime == 0) steptime = stepdur

            -- todo: change this to just be if there are any commands?
            if steptime == stepdur then    
                add_snake_commands(batch)
                add_grow_commands(filterpairs(mushtab.items, filtermush))
                add_grow_commands(filterpairs(mushtab.items, filterpoison))
                check_doors()
                add(pl.commands, batch_command(batch, true))
                willadd_shrooms, willdel_shrooms,changedirt,willblock,willmoveto = {},{},0,nil
            end
        else
            steptime = stepdur      
        end
    end
end

function filtermush(k, v)
    return v.type == 26
end

function filterpoison(k, v)
    return v.type == 12
end

function check_doors(startup)
    for _, door in pairs(doortab.items) do
        local change, full, hc = 0
        if door.isgoal then
            local dct = pairscount(dirttab.items)
            change =  pairscount(willadd_shrooms) - pairscount(willdel_shrooms)
            full = pairscount(mushtab.items) + change == dct + changedirt
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
            full = mushtab.itemcount(flag) + change == dirttab.itemcount(flag)
        end

        if full then
            if not door.open then 
                add(door.commands, setdoor_command(door))
                hc = true
            end
        elseif door.open then -- close
            local _, other = doorpair(door.cell)
            local s1,s2 = is_snake(stepcell(door.cell, "d"), true),other and is_snake(stepcell(other.cell, "d"), true)
            if not s1 or not s2 or s1.snake ~= s2.snake then
                add(door.commands, setdoor_command(door, true))
                hc = true
            end
        end
        if (not hc) add(door.commands, command()) -- empty
        if (startup and full) door.open, door.spr = true, door.isgoal and 17 or 18
    end
end

function killsnake_command(snake, killtype)
    snake.willdie = true
    local c = command()
    local s,n = c.start,c.next
    c.start = function()
        snake.willdie,snake.fadeout = false,true
        s()
        if undo then 
            snaketab.add(snake)
            if (killtype == "shovel") shovels += 1
        else 
            snake_dissolve(snake, killtype)
            local ksfx = 12
            if killtype == "shovel" then
                shovels -= 1
                sfl_c,sfl_t = 1,4
            else
                ksfx = killtype == "poison" and 19 or 21
            end
            playsfx(ksfx,3)
        end
    end
    c.next = function()
        n()
        if (not undo) snaketab.del(snake.cell)
        snake.fadeout = nil
    end
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
            playsfx(10, 3)
            shovels -= 1
            if dt == 1 then
                grasstab.del(cell)
                createdirt(cell, 1)
            else
                mushtab.del(cell)
                dirttab.del(cell)
                createitem(cell, 22, nil, holetab)
            end
            dissolve_cluster(cellpos(cell), 9, 6, 2.4, .25)
        end
    end
    return c
end

function can_place_mush(cell)
    return (
        dirttab.item(cell) and 
        not willadd_shrooms[vecstring(cell)] and -- bug? willadd_shrooms always emptied by this point
        not mushtab.item(cell) and 
        not blocktab.item(cell) and
        not is_snake(cell) and
        not shoveltab.item(cell)
    )
end

function stepperc()
    return (stepdur - steptime) / stepdur
end

function refresh_hud()
    if (undo) placehud_pos = nil
    local facecell = stepcell(pl.cell, pl_dir)
    if can_place_mush(facecell) then 
        new_placehud(facecell, 10)
    end
    local dtype = digtype(facecell)
    if dtype == 1 or dtype == 22 then
        new_placehud(facecell, 6)
    end
    local seg = canattack(facecell)
    if (seg) new_placehud(seg.cell, 6)
end

function new_placehud(cell, sprite)
    placehud_pos = cellpos(cell)
    placehud_spr = sprite
    placehud_clock = clock(1.79)
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
    if (shovels > 0) then 
        return is_snake(cell)
    end
end

-- draws

function draw_stepfade(thing)
    if thing then 
        stepfade()
        draw_item(thing)
        pal()
    end
end

function _draw()
    cls()
    -- border
    pal()
    spr(14, 0, 0, 2, 2, true) 
    spr(14, 112, 0, 2, 2) 
    spr(14, 0, 112, 2, 2, true, true) 
    spr(14, 112, 112, 2, 2, false, true) 
    for i=1,6 do
        spr(13, 16*i, 0, 2, 1)
        spr(13, 16*i, 120, 2, 1, false, true)
        spr(31, 120, 16*i, 1, 2)
        spr(31, 0, 16*i, 1, 2, true, false)
    end

    if transition_state then 
        for i=1,16 do
            local perc = transition_clock.perc
            pal(i - 1, lerptab(fades[i], transition_state == "out" and perc or 1 - perc))
        end
        if (transition_animate) transition_animate()
    end
    if state == "puzzle" then
        if not windelay and not hidemain then
            foreach({grasstab, dirttab, watertab, waterblocktab, holetab, baseobstacletab, shoveltab, signtab}, draw_items_ct)
            draw_items(mushtab.items, nil, mushspr)
            draw_items_ct(blocktab)
            -- snakes
            for _, s in pairs(snaketab.items) do
                if (s.fadeout ~= nil and not s.fadehead) stepfade(s.fadeout)
                for b in all(s.body) do
                    local p = cellpos(b.cell)
                    if (b.mask) celmask(p)
                    spr(b.spr, p.x, p.y)
                end
                if (s.fadeout ~= nil and s.fadehead) stepfade(s.fadeout)
                if (s.mask) celmask(s.pos)
                spr(s.spr, s.pos.x, s.pos.y, 1, 1)
                if (s.fadeout ~= nil) pal()
            end
            draw_items(doortab.items, isnotgoal)
            draw_stepfade(outblock)
            draw_stepfade(outsnake)
            -- hud
            if placehud_pos then
                if placehud_clock and placefx(placehud_clock.perc, placehud_pos) then
                    spr(placehud_spr, placehud_pos.x, placehud_pos.y)
                    pal()
                end
            end
            if shovels > 0 then
                spr(6, shovtarg.x, shovtarg.y)
                if shovels > 1 then
                    print("+"..shovels-1, 104, 7, 6)
                end
            end 
        end
        -- above transition
        if (not forcefade) pal()
        spr(goaldoor.spr, goaldoor.pos.x, goaldoor.pos.y)
        if signtext and signpos then 
            local x = lerp(signpost_pos.x, 8, sign_t, smooth)
            local y = lerp(signpost_pos.y, signto.y-8, sign_t, smooth)
            clip(x, y, max(112*sign_t,6), max(32*sign_t,3))
            rectfill(0, 0, 128, 128, 0)
            map(56,30,x,y,14,4)
            print(signtext, signpos.x, signpos.y, 9)
            if (signtext_2) print(signtext_2, hcenter(signtext_2, 64), signpos.y+10, 9)
            clip()
        end
        -- player
        if not died then
            if (pl.fadetype) draw_fade(stepperc(), pl.fadetype)
            pl_spr = playersprites[pl_dir][pl_animclock.t] 
            if not won then
                if (pl_mask) celmask(pl.pos)
                spr(pl_spr, pl.pos.x, pl.pos.y, 1, 1, pl_dir == "r")  
            end  
            if (pl.fadetype) pal()
        end

        draw_stepfade(outplayer)
        draw_dissolve()
        
        if wonspark_pos then
            for i=0, wonspark_parts-1 do
                local t = wonspark_clock.perc
                local r,a,c = lerp(wonspark_startrad, wonspark_endrad, t) - i/wonspark_parts*5, lerp(wonspark_startang, wonspark_endang, t) + i/wonspark_parts, i%2==0 and 10 or 7
                local pos,f = wonspark_pos + pvec(r, a), lerptab(fades[c+1], t)
                print("\146", pos.x-4, pos.y-3, f)
            end
        end
    elseif state == "heading" then
        print(pz_name, hcenter(pz_name, 64), 20, 9)
        if (from_title) pal()
        draw_mountain()
        if (transition_animate) draw_title_foreground()
    elseif state == "title" then
        spr(48, 10, 16, 14, 1)
        spr(91, 10, 24)
        spr(91, 109, 24, 1, 1, true)
        local text = "\151 to begin"
        print(text, hcenter(text, 64), 30, 9)
        -- above fade if not transitioning in
        if (transition_state ~= "in") pal()
        draw_mountain()
        draw_title_foreground()
    end
    -- screenflash
    if sfl_t and sfl_t > 0 then 
        sfl_t -= 1
        if sfl_t % 2 == 0 then
            pal()
        else
            pal(0, sfl_c, 1)
        end
    end
end

function draw_mountain()
    clip(4,4,120,120)
    spr(240, clouds_x, clouds_y, 16, 1)
    spr(240, clouds_x + 128, clouds_y, 16, 1)    
    rectfill(4, clouds_y + 8, 124, 128, 6)
    palt(0, false)
    palt(3, true)
    map(24, 27, 0, mountain_y, 16, 10)
    palt()
    rectfill(4, mountain_y+80, 124, 124, 1)
    local i=0
    for n in all(mapnodes) do
        local p = n.p + vec(0, mountain_y)
        if state == "heading" then 
            local cx,cy = p.x,p.y+sin(i*.1)*4
            if n.s == pz_i then
                rectfill(cx,cy,cx+2,cy+2,lerptab(tb"12,13,2,1,2,13,12,", gt.perc))
            elseif n.s < pz_i then
                spr(10,cx,cy-4)
            end            
        end
        i+=1
    end
    clip()
end

function draw_title_foreground()
    clip(4, 40, 122, 84)
    palt(0, false)
    palt(3, true)
    pal(1, 0)
    map(0, 25, tmlx, 40, 12, 12)
    map(12, 25, tmrx, 40, 12, 12)
    pal()
    circfill(64, door_y + 108, 85, 0) 
    map(74, 26, 16, door_y, 12, 7)
    clip()
end

function snakebodyspr(a, b, c)
    local adir = dircell(b, a)
    local cdir = c and dircell(b, c)
    if (not adir or not cdir) return 36 -- this is bug?
    local result = bodylut[adir..nilstring(cdir)]
    return bodylut[adir..nilstring(cdir)]
end

function dim()
    for c=2,15 do
        pal(c, 1)
    end
end

function placefx(perc, pos)
    if perc then 
        if ceil(10*perc)%2 == 0 then
            celmask(pos)
            dim()
            return true
        end
    end    
    return false
end

function celmask(p, c)
    rectfill(p.x, p.y, p.x+7, p.y+7, c or 0)
end

function draw_item(item)
    if (item) draw_items{item}
end

function draw_items(tab, condition, selectspr) 
    for _, i in pairs(tab) do
        local draw, s = true, selectspr and selectspr(i) or i.spr
        if (condition) draw = condition(i)
        if draw then
            local p = i.pos
            if (i.mask) celmask(p)
            if (i.standcrate) pal(9,4)
            if (i == inblock) stepfade(true)
            spr(s, p.x, p.y)
            if (i == inblock) pal()
            if (i.standcrate) pal()
        end
    end
end

function draw_items_ct(tab)
    draw_items(tab.items)
end

playersprites = { 
    u = tb"25,9,",
    d = tb"24,8,",
    l = tb"23,7,",
    r = tb"23,7,"
}

-- put draw operations b/w draw_fade() and a call to pal()
function draw_fade(t, fadetype)
    for c=0,15 do
        pal(c, lerptab(fades[c + 1], fadetype == "in" and 1-t or t))
    end
end

function stepfade(out)
    local type = out and "out" or "in"
    if (undo) type = out and "in" or "out"
    draw_fade(stepperc(), type)
end

function loadmap()
    signtab,baseobstacletab,mushtab,grasstab,dirttab,doortab,blocktab,shoveltab,snaketab,holetab,watertab,waterblocktab,snakecontext,pl,mapgrid = celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),celltab(),{},{},vec(pz_w, pz_h)

    for mx = pz_x, pz_x + (pz_w - 1) do
        for my = pz_y, pz_y + (pz_h - 1) do
            loadobj(mget(mx, my), mx - pz_x, my - pz_y)
        end
    end
    if (pz_name == "east river fork") createitem(vec(6,8), 46, nil, waterblocktab)

    -- create snakes
    for _, obj in pairs(snakecontext) do
        local cell, index = obj.cell, obj.index
        -- create snake starting at head, following body
        if index >= 32 and index <= 35 then
            creategrass(cell)
            local idx, body, i, lastdir, lastcel = 1, {}, index
            while i ~= 42 do
                local dir
                if i == 32 then dir = "d"
                elseif i == 33 then dir = "l"
                elseif i == 34 then dir = "u"
                elseif i == 35 then dir = "r"
                elseif i == 36 then dir = lastdir == "u" and "u" or "d"
                elseif i == 37 then dir = lastdir == "l" and "l" or "r"
                elseif i == 40 then dir = lastdir == "r" and "u" or "l"
                elseif i == 41 then dir = lastdir == "l" and "u" or "r"
                elseif i == 38 then dir = lastdir == "r" and "d" or "l"
                elseif i == 39 then dir = lastdir == "l" and "d" or "r" end
                lastcel = cell
                cell = stepcell(cell, dir)
                i = snakecontext[vecstring(cell)].index
                local sprite,idir = i,dircell(lastcel,cell)
                if (i == 42) sprite = (idir == "r" or idir == "l") and 37 or 36
                add(body, {
                    cell=cell, 
                    pos=cellpos(cell), 
                    spr=sprite, 
                    mask=true
                })
                creategrass(cell)
                lastdir = dir
                idx += 1
            end
            local sn = {
                body = body,
                cell = obj.cell,
                pos = cellpos(obj.cell),
                mask = true
            }
            for b in all(body) do
                b.snake = sn
            end
            sn.snake,sn.facing = sn,dircell(sn.body[1].cell, sn.cell) or "d"
            sn.spr = snakespr(sn.facing)
            snaketab.add(sn)
        end
    end
    snakecontext = nil

    check_doors(true)
    refresh_hud()
end

function reset_puzzle()
    -- bugfixes: reset other things here too, or save tokens if can do in loadmap
    loadmap()
end

function is_dirt_type(i)
    return i == 1 or i == 5 or i == 21
end

function createdirt(cell, spr) 
    return createitem(cell, spr, tostr(spr), dirttab, nil)
end

function creategrass(cell)
    return createitem(cell, 2, nil, grasstab, nil)
end

function loadobj(i, gx, gy)
    local cell = vec(gx, gy)
    if contains(tb"7,43,6,", i) then
        creategrass(cell)
    end

    if is_dirt_type(i) then
        createdirt(cell, i)
    elseif i == 2 then
        creategrass(cell)
    elseif i == 43 then
        createitem(cell, i, nil, blocktab, true)
    elseif i == 16 then
        sign,signpost_pos,sign_texts = createitem(cell, i, nil, signtab),cellpos(cell),pz[6] or {}
    elseif i == 4 then
        createdoor(cell, tostr(5))
    elseif i == 20 then
        createdoor(cell, tostr(21))
    elseif i == 19 then 
        goaldoor = createitem(cell, 19, flag, doortab)
        goaldoor.open,goaldoor.isgoal,goaldoor.orgspr,goaldoor.openspr = false,true,19,17
    elseif i == 7 then
        pl.cell,pl.pos,pl.commands,pl_spr,pl_mask,pl_dir,pl_animclock = cell,cellpos(cell),{},7,true,"r",clock(2, true)
    elseif i == 26 or i == 12 then
        createdirt(cell, 1)
        createmush(cell, 1, mushtab, i == 12 and 2 or 1, i)
    elseif i == 6 then
        createitem(cell, i, nil, shoveltab, true)
    elseif i >= 62 and i <= 75 then
        createitem(cell, i, nil, watertab)
    elseif i >= 32 and i <= 42 then
        -- create snakes after collecting context
        snakecontext[vecstring(cell)] = {cell=cell,index=i}
    elseif i and i ~= 0 then
        createitem(cell, i, nil, baseobstacletab)
    end
end

function snakespr(facing)
    if (facing == "u") return 32
    if (facing == "r") return 33
    if (facing == "d") return 34
    return 35
end

function createitem(cell, spr, flag, ctab, mask)
    local obj = {
        cell = cell,
        pos = cellpos(cell),
        spr = spr,
        flag = flag,
        commands = {},
        mask = mask
    }
    if (ctab) ctab.add(obj)
    return obj
end

function createmush(cell, flag, ctab, t, type)
    local obj = createitem(cell, 0, flag, ctab)
    obj.t,obj.mask,obj.type = t or 0,true,type or 26
    return obj
end

function createdoor(cell, flag)
    local orgspr, obj = flag == tostr(5) and 4 or 20, createitem(cell, orgspr, flag, doortab)
    obj.orgspr,obj.openspr,obj.spr = orgspr,18,orgspr
    return obj
end

function update_transition()
    clocktick(transition_clock)
    if transition_clock.perc == 1 then
        transition_state = nil
        if (transition_next) transition_next()
        transition_next,transition_clock = nil,clock(.66)
    end
end

function commands()
    local stacks = {pl.commands}
    local addcommands = function(celltab)
        for _, i in pairs(celltab.items) do 
            if (#i.commands > 0) add(stacks, i.commands)
        end
    end
    for tab in all{mushtab, doortab} do
        addcommands(tab)
    end
    return stacks
end

function command()
    return {
        start = function() end,
        run = function(t) end,
        next = function() end
    }
end

function batch_command(commands, reverse_undo)
    local cmd = command()
    cmd.reverse_undo = reverse_undo
    cmd.start = function()
        if undo and cmd.reverse_undo then
            doreverse(commands, function(c) c.start() end)
        else
            for c in all(commands) do
                c.start()
            end
        end
    end
    cmd.run = function(t)
        if undo and cmd.reverse_undo then
            doreverse(commands, function(c) c.run(t) end)
        else
            for c in all(commands) do
                c.run(t)
            end
        end
    end
    cmd.next = function()
        if undo and cmd.reverse_undo then
            doreverse(commands, function(c) c.next() end)
        else
            for c in all(commands) do
                c.next()
            end
        end
    end
    return cmd
end

function getshovel_command(nextshovel)
    local c, sp = command(), nextshovel.pos
    c.start = function()
        if undo then
            shovels -= 1
            shoveltab.add(nextshovel)
        else    
            playsfx(18, 3)
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

function doreverse(tab, fx)
    local count = #tab
    for i=#tab,1,-1 do
        fx(tab[i])
    end
end

function setdoor_command(door, close)
    local c, s, os, prevspr, prevopen = command(), door.orgspr, door.openspr, door.spr, door.open
    local nextspr = close and s or os
    c.start = function()
        door.spr = undo and prevspr or nextspr 
        if undo then
            door.open = prevopen
        else
            door.open = not close
        end
        if not undo then
            if door.open ~= prevopen then
                if close then
                    playsfx(13, 3)
                else
                    playsfx(5, 3)
                end
            end
        end
    end
    return c
end

function thrudoor_command(incell)
    local c, _, outdoor  = moveplayer_command(stepcell(incell, "d"), incell), doorpair(incell)
    local outdoor_cell = outdoor.cell
    local outdoor_landing, outplayer_local, prevdir = stepcell(outdoor_cell, "d"), createitem(outdoor_cell, 8), pl_dir
    c.start = function()
        pl_dir, outplayer, invertcontrols, pl_mask = "u", outplayer_local, not undo, false
    end
    local n = c.next
    c.next = function()
        n()
        pl_dir, pl_mask = undo and prevdir or "d", true
        if (not undo) pl.cell, pl.pos = outdoor_landing, cellpos(outdoor_landing)
        outplayer = nil
    end
    return batch_command{c, movecell_command(outplayer_local, outdoor_cell, outdoor_landing), fade_command(pl)}
end


function add_snake_commands(batch)
    local kills = {}
    for _, snake in pairs(snaketab.items) do
        if not snake.willdie then
            local seg, move = is_snake(stepsnake(snake, true), true, snake), movesnake_command(snake, snake.cell, stepsnake(snake))
            add(batch, move)
            if (seg) add(kills, seg.snake)
        end
    end
    for s in all(kills) do
        add(batch, killsnake_command(s, "snake"))
    end
end

function stepsnake(snake, usedoors)
    local snakecell = snake.cell
    local ahead, toright, toleft, behind = stepcell(snakecell, snake.facing), stepcell(snakecell, turn(snake.facing, true)), stepcell(snakecell, turn(snake.facing)), stepcell(snakecell, turn(turn(snake.facing)))
    local moveto = behind
    if not is_snake_obstacle(snake, ahead) then
        moveto = ahead
    elseif not is_snake_obstacle(snake, toright) then
        moveto = toright
    elseif not is_snake_obstacle(snake, toleft) then
        moveto = toleft
    end
    if usedoors then
        local indoor, outdoor = doorpair(moveto)
        if indoor and indoor.open and not indoor.isgoal and dircell(snake.cell, indoor.cell) == "u" and not is_snake_obstacle(snake, stepcell(outdoor.cell, "d")) then
            moveto = stepcell(outdoor.cell, "d")
        end
    end
    return moveto
end

function is_snake_obstacle(snake, cell) 
    local isblank,willdig,obstacle,indoor,outdoor = #items(cell) == 0,willdig_at and digtype(cell) == 22,blocktab.item(cell) or shoveltab.item(cell),doorpair(cell)
    if (watertab.item(cell) and not waterblocktab.item(cell)) obstacle = true
    if indoor then
        if indoor.open and not indoor.isgoal then 
            obstacle = dircell(snake.cell, indoor.cell) ~= "u" 
            obstacle = obstacle or is_snake_obstacle(snake, stepcell(outdoor.cell, "d"))
        else 
            obstacle = true
        end
    end
    if (willblock) obstacle = obstacle or willblock == cell
    return isblank or obstacle or willdig or not inbounds(cell)
end

function is_snake(at, stepahead, exclude)
    for _, s in pairs(snaketab.items) do
        if s ~= exclude then
            local cel = stepahead and stepsnake(s, true) or s.cell
            if (cel == at) return s

            local i = 1
            for b in all(s.body) do
                local bcel = b.cell
                if (stepahead) bcel = i > 1 and s.body[i-1].cell or s.cell
                if (bcel == at) return b
                i += 1
            end
        end
    end
end

function turn(dir, cw)
    local tu = cw and tb"u=r,r=d,d=l,l=u," or tb"u=l,r=u,d=r,l=d,"
    return tu[dir]
end

function add_grow_commands(shrooms)
    for m in all(shrooms) do
        local commands = {}
        if not willdel_shrooms[vecstring(m.cell)] then
            if m.t == 0 or (m.t == 1 and m.type == 12) then
                add(commands, growmush_command(m))
            else -- expand colony
                for d in all(tb"u,d,l,r,") do
                    local at = stepcell(m.cell, d)
                    local items = items(at)
                    local existing = items[1] 
                    local blocked = #items ~= 1 or
                        (existing and not is_dirt_type(existing.spr)) or
                        (willmoveto and at == willmoveto) or 
                        (willadd_shrooms[vecstring(at)] ~= nil) or 
                        is_snake(at) or 
                        is_snake(at, true)

                    local ispoison = existing and existing.type == 12
                    local squashpoison, valid = ispoison and m.type == 26, existing and not blocked and inbounds(at)
                    if valid or squashpoison then
                        if (squashpoison) add(commands, delmush_command(existing))
                        local c = addmush_command(at, 0, m.type)
                        local cstart = c.start
                        c.start = function()
                            cstart()
                            if (not undo) playsfx(8, 2, true)
                        end
                        add(commands, c)
                    end
                end
            end
        end
        if #commands > 0 then
                add(m.commands, batch_command(commands, true))
            else
                add(m.commands, command()) -- empty
        end
    end
end

function addmush_command(at, t, type)
    local c = command()
    local m = createmush(at, dirttab.item(at).flag, nil, nil, type)
    m.t = t or 0
    willadd_shrooms[vecstring(at)] = m

    c.start = function()
        if (not undo) mushtab.add(m)
    end
    c.next = function()
        if (undo) mushtab.del(at)
    end

    return c
end

function growmush_command(mush)
    local c = command()
    c.start = function()
        local t = mush.t
        mush.t = undo and t - 1 or t + 1
        if (not undo) playsfx(8, 2, true)
    end
    return c
end

function placemush_command(cell)
    local c = command()
    c.start = function()
        if not undo then 
            playsfx(4, 2)
        end
    end

    local addmush = addmush_command(cell, 0)
    return batch_command{c, addmush}
end

function playsfx(n, c, shy)
    if (shy and contains(sfxc, c)) return
    add(sfxc, c)
    sfx(n, c)
end

function delmush_command(mush, pick)
    willdel_shrooms[vecstring(mush.cell)] = mush
    local c,t,prevcommands = command(),mush.t,mush.commands
    c.start = function()
        if not undo then
            if (pick) playsfx(3, 3, true)
            mushtab.del(mush.cell)
        end
    end
    c.next = function()
        if undo then
            local m = createmush(mush.cell, dirttab.item(mush.cell).flag, mushtab, t, mush.type)
            m.commands = prevcommands
        end
    end
    return c
end

function fade_command(obj, fadein)
    local c = command()
    c.start = function ()
        obj.fadetype = fadein and (undo and "out" or "in") or (undo and "in" or "out")
    end
    c.next = function()
        obj.fadetype = nil
    end
    return c
end

function win_command(door)
    local create_windelay = function()
        windelay = clock(1)
    end
    local c = command()
    c.next = function()
        playsfx(6, 3)
        won,wonspark_pos,wonspark_clock,wonspark_parts,wonspark_startang,wonspark_endang,wonspark_startrad,wonspark_endrad,transition_state,transition_next = true,door.pos+vec(4,4),clock(2.5),10,.25,2,3,36,"out",create_windelay 
    end
    return c
end

function die_command() 
    local c = command() 
    c.next = function()
        died,die_time = true,21
    end
    return c
end

function doorpair(cell)
    local a = doortab.item(cell) 
    if a then
        for _, d in pairs(doortab.items) do 
            if d.flag == a.flag and d ~= a then
                return a, d
            end
        end
        return a
    end
end

function movepushblock_command(pushblock, to)
    local from,batch = pushblock.cell,{}
    local moveblock = movecell_command(pushblock, from, to)
    add(batch, moveblock)
    local n = moveblock.next

    local indoor, outdoor, outdoor_stepcell, outblock_local = doorpair(to)
    if indoor then
        outdoor_stepcell = stepcell(outdoor.cell, "d")
        -- todo crash: outdoor can be nil when pushing 2 blocks up against a goal door
        outblock_local = createitem(outdoor.cell, 43)
        add(batch, movecell_command(outblock_local, outdoor.cell, outdoor_stepcell))
    end
    local tocell, towater, towaterblock = outdoor_stepcell or to, watertab.item(to), waterblocktab.item(to)

    moveblock.start = function()
        if (indoor) outblock, inblock = outblock_local, indoor and pushblock
        if towater then 
            pushblock.mask = false
            if undo and not blocktab.item(to) then
                waterblocktab.del(to)
                blocktab.add(pushblock)
            end
            if not towaterblock then 
                pushblock.spr, pushblock.inwater = 46, not undo
            end
        elseif not undo then
            playsfx(20, 3)
        end
    end

    moveblock.next = function()
        n()
        if towater and not undo and not towaterblock then
            blocktab.del(from)
            createitem(to, 46, nil, waterblocktab)
        else
            blocktab.move(pushblock, undo and tocell or from, undo and from or tocell)
            if undo then
                if towater then 
                    pushblock.spr = 43
                end
                if not watertab.item(from) then
                    pushblock.mask = true
                end 
            else
                pushblock.cell, pushblock.pos, pushblock.mask = tocell, cellpos(tocell), not watertab.item(to)
                local mush = mushtab.item(tocell)
                if mush then
                    mushtab.del(mush.cell)
                end
            end
            if (indoor) outblock, inblock = nil
        end
    end

    local mush = mushtab.item(tocell)
    if (mush) add(batch, delmush_command(mush))
    if (towater and not towaterblock) add(batch, splash_command(pushblock, to))
    return batch_command(batch)
end

function signtarget(text)
    return vec(hcenter(text, 64), 15)
end

function moveplayer_command(from, to)
    local c = movecell_command(pl, from, to)
    local n, r, lastmask, d, towater, fromwater, towaterblock, fromwaterblock = c.next, c.run, pl_mask, dircell(from, to), watertab.item(to), watertab.item(from), waterblocktab.item(to), waterblocktab.item(from)
    c.dir = pl_dir
    c.start = function()
        pl_dir = d
        clocktick(pl_animclock, undo and -1 or 1)
        if towater or goaldoor.cell == to then
            pl_mask = false
        end
        if (fromwater and undo) pl_mask = false
        if (fromwaterblock) fromwaterblock.standcrate = false
        if (towaterblock and undo) towaterblock.standcrate = false
    end
    c.next = function()
        n()
        if undo then 
            pl_dir, pl_mask = c.dir, lastmask
            if (fromwaterblock) fromwaterblock.standcrate = true
        else
            if (fromwater and not towaterblock) pl_mask = true
            if (towaterblock) towaterblock.standcrate = true
        end
    end
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
    c.run = function (t) 
        obj.pos = lerpvec(undo and to or from, undo and from or to, t, sineout)
    end
    return c
end

function movesnake_command(snake, from, to)
    local batch, indoor, outdoor = {}, doorpair(to)
    local waterblockto, outdoor_stepcell, outsnake_local = waterblocktab.item(to)
    if indoor and not indoor.isgoal then
        outdoor_stepcell, outsnake_local = stepcell(outdoor.cell, "d"), createitem(outdoor.cell, 34)
        add(batch, movecell_command(outsnake_local, outdoor.cell, outdoor_stepcell))
    end
    local tocell = outdoor_stepcell or to

    local c = move_command(snake, cellpos(from), cellpos(to))
    add(batch, c)
    local n, lastbody, dir, lastspr, lastfacing, mush = c.next, snake.body, dircell(from, to), snake.spr, snake.facing, mushtab.item(tocell)
    mush = mush or willadd_shrooms[vecstring(tocell)]
    
    c.start = function()
        if (outsnake_local) outsnake,snake.fadeout,snake.fadehead = outsnake_local,true,true
        if undo then
            snake.body, snake.facing = lastbody, lastfacing
            if (waterblocktab.item(from)) snake.mask = false
        else
            if (waterblockto) snake.mask = false
            local i,nextbody = 0,{}
            for _ in all(lastbody) do
                local seg = lastbody[i]
                if i == 0 then 
                    seg={
                        cell=from, 
                        pos=cellpos(from),
                        spr=snakebodyspr(to, from, lastbody[1].cell),
                        snake=snake,
                        mask=true
                    }
                end
                if (waterblocktab.item(seg.cell)) seg.mask = false
                add(nextbody, seg)
                i += 1
            end
            snake.body, snake.spr = nextbody, dir and snakespr(dir) or 34
        end
        snake.cell = undo and from or tocell -- set the cell immediately, for particles
    end

    c.next = function()
        n()
        snaketab.move(snake, undo and tocell or from, undo and from or tocell)
        if undo then
            snake.spr = lastspr
            if (not waterblocktab.item(from)) snake.mask = true
        else
            if (outsnake) snake.spr = 34
            if (not waterblockto) snake.mask = true
        end
        if (outsnake_local) outsnake,snake.fadeout,snake.fadehead = nil
        if (not undo) snake.facing = outsnake_local and "d" or dircell(snake.body[1].cell, snake.cell)
    end

    if mush then
        local ispoison = mush.type == 12
        add(batch, delmush_command(mush, ispoison==false))
        if ispoison then
            add(batch, killsnake_command(snake, "poison"))
        end
    end

    return batch_command(batch, true)
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
    local inc = (increment ~= nil) and increment or 1
    c.t = wrap(c.t + inc, 1, c.maxt, true)
    c.perc = c.t / c.maxt
end


function contains(collection, item)
    for i in all(collection) do
        if (i == item) return true
    end
    return false
end

function pairscount(tab)
    local c = 0
    for _ in pairs(tab) do
        c += 1
    end
    return c
end

function wrap(n, low, high, int)
    if (n < low) return high - (int and (low-n)-1 or low-n)
    if (n > high) return low + (int and (n-high)-1 or n-high)
    return n
end

function hcenter(s, center)
    return center - #s*2
end

function pointsize(cell)
    return scalevec(cell, 8)
end

function cellpos(cell)
    return gridorg() + pointsize(cell)
end

function gridorg()
    local s = pointsize(mapgrid)
    return vec((128 - s.x) / 2, (128 - s.y) / 2)
end

function pollnext(cell)
    if (btn(0)) return stepcell(cell, "l")
    if (btn(1)) return stepcell(cell, "r")
    if (btn(2)) return stepcell(cell, invertcontrols and "d" or "u")
    if (btn(3)) return stepcell(cell, "d")
    return cell
end

function stepcell(c, dir)
    return vec(
        dir == "l" and c.x - 1 or (dir == "r" and c.x + 1 or c.x), 
        dir == "u" and c.y - 1 or (dir == "d" and c.y + 1 or c.y)
    )
end

function inbounds(c)
    return c.x >= 0 and c.y >= 0 and c.x < mapgrid.x and c.y < mapgrid.y
end

function dircell(from, to)
    local x1, y1, x2, y2 = from.x, from.y, to.x, to.y
    if y1 == y2 then
        if x1 > x2 then
            return "l"
        elseif x1 < x2 then
            return "r"
        end
    end
    if x1 == x2 then
        if y1 > y2 then
            return "u"
        elseif y1 < y2 then
            return "d"
        end
    end
end

function celltab()
    local tab = {items={},flagmap={}}
    tab.add = function(item)
        local cellkey = vecstring(item.cell)
        tab.items[cellkey] = item
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
        local cur = tab.item(from)
        tab.items[vecstring(to)] = obj
        if (obj == cur) tab.items[vecstring(from)] = nil
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

function items(cell)
    local result = {}
    if (cell == pl.cell) add(result, pl)
    for tab in all{mushtab, grasstab, dirttab, doortab, blocktab, snaketab, shoveltab, watertab, waterblocktab, signtab} do
        local item = tab.item(cell)
        if (item) add(result, item)
    end
    return result
end

function mushspr(item)
    if item.type == 12 then
        local tab = tb"11,27,"
        return tab[item.t+1] or 12
    end
    return item.t == 0 and 10 or 26
end

lastdowns = tb"0,0,0,0,0,0,0,"
function update_lastdowns()
    for i=0,5 do
        lastdowns[i] = btn(i)
    end
    lastdowns[6] = btn()
end

function btnd(b) 
    if (not b) return (btn() > 0 and lastdowns[6] == 0 )
    return (btn(b) and (lastdowns[b] == false))
end

function btnu(b)
    return (not btn(b) and (lastdowns[b] == true))
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

function scalevec(v1, s)
    return vec(v1.x * s, v1.y * s)
end

function pvec(r, ang)
    return scalevec(vec(cos(ang), sin(ang)), r)
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
    return t*t*t*(t*(6*t-15)+10)
end

function log(s)
    printh(s, "mush")
end

function boolstring(b) 
    if b ~= nil then
        return b and "true" or "false"
    end
    return "nil"
end

function nilstring(s)
    return s ~= nil and s or "nil"
end

function vecstring(v)
    return "("..(v and v.x or "nil")..", "..(v and v.y or "nil")..")"
end

__gfx__
00000000000000000000000000000000009999000000000000007770000222000022000000220000000000000000000000000000000000100010000000011111
00000000000000000000000000000000094444900000000000076665002222200222200002222000000000000000000000770000000010010010110010100001
00700700040004000000000000000000944494490000400000766665022222220222220002222200000000000000000007777000100001010001001010006c01
00077000000000000000b030000000009449944900044000000666650d222002202dd2002022220000ccc000000000000020000000111100011000000100c101
000770000040004000000300000000009449444900040000004266500dd00000000dd000000220000ccccc000077700000807770000000000000000000010001
00700700000000000b0300000000000094499449000440000421050000000d0000d0000000000d00000100000002000000800200000000000000000000000010
0000000004000400003000000000000094444449000000004210000000d2200000022d0000d22000000c00000008077000080800000000000000000000001000
00000000000000000000000000000000999999990000000001000000000000000000000000000000000000000000000000000000000000000000000000000110
00000000700770090077990090099009009999000000000000000000000000002000000020000000000000000000000000000000000000000000000000000000
0000000077779999070000909999999909444490000000000004400000002222222000002220000000cccc000000000000000000000000000000000000000100
044444407000000970000009988998899444444900000000004224000022222002222000022220000cccccc00000000000000003000000000000000000000010
04222240700000097000000998899889944994490004400004211240022222000022220000222200000100000077700000000003000000000000000000000010
04444440700000097000000998a99a899444944900004000041001400d22d000002dd200002222000000c0000002000000000030300000000000000000000100
000220007000000970000009982992899449444900040000004004000dd00000000ddd0000d220000000c0000008087000000333330000000000000000001011
000440007000000970000009988998899444444900000000000440000022d00000d2200000022d00000c00000008080000000330330000000000000000001000
00000000700000097000000999999999999999990000000000000000000000000000000000000000000000000000000000003003003000000000000000000000
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
41111114000000004400004411111111000000444400000041111111111111144111111444000000000000444400004400000000000000000000000000000000
41111114000440001144441111111111000444111144400041111111111111144111111411444000000444111144441100000000000000000000000000000000
04111140004114001111111111111111004111111111140004111111111111400411114011111400004111111111111100000000000000000000000000000000
04111140041111401111111111111111041111111111114004111111111111400411114011111140041111111111111100000000000000000000000000000000
04111140041111401111111111111111041111111111114004111111111111400411114011111140041111111111111100000000000000000000000000000000
04111140041111401111111111111111041111111111114000411111111114000041140011111400004111111111111100000000000000000000000000000000
41111114411111141144441111444411411111111111111400044411114440000004400011444000000444111111111100000000000000000000000000000000
41111114411111144400004444000044411111111111111400000044440000000000000044000000000000441111111100000000000000000000000000000000
00000000033333330333333311110333003333333333333333333333333333003330111133333330333333309900000001999888889998892988999888889991
00000000103333330333333311110333110033333333333333333333333300113330111133333330333333019900000001999889998888892988888999889991
00000000110333331033333311111033111100333333333333333333330011113301111133333301333330119900000001999898888888892988888888989991
00000000111033331033333311111033111111003333333333333333001111113301111133333301333301119900000000999898888888892988888888989991
00000000111103331103333311111103111111110033333333333300111111113011111133333011333011119900000000199889888888892988888889889910
00000000111110331103333311111103111111111100333333330011111111113011111133333011330111119000000000199888988888892988888898889910
00000000111111031110333311111110111111111111003333001111111111110111111133330111301111110900000000199888988888892988888898889910
00000000111111101110333311111110111111111111110000111111111111110111111133330111011111119000000000199889888888892988888889889910
00333333333333000333333333333330111103333330111100033333333330003333333333333333333333300333333300199898888888892988888888989910
11003333333300111033333333333301111110333301111111100033330001113333333333333333333333300333333300019898888888892988888888989100
11110333333011111103333333333011111111033011111111111103301111113333333333333333333333011033333300019898888889892989888888989100
11111033330111111103333333333011111111033011111111111110011111113333333333333333333333011033333300019898888899892989988888989100
11111103301111111110333333330111111111100111111111111111111111110000333333330000333330111103333300199898888898892988988888989910
11111103301111111110333333330111111111100111111111111111111111111111003333001111333301111110333300199898888899892989988888989910
11111110011111111110333333330111111111100111111111111111111111111111110330111111330011111111003300199898888882892982888888989910
11111110011111111110333333330111111111100111111111111111111111111111111001111111001111111111110000199898888888892988888888989910
33333333333333333131313111110111111111111111111111111111111111111111110011111111000000000000000001199898888888892988888888989910
33333333333333331313131311100011100111111111100111111111011111101111001111111111000000000000000001999898888888892988888888989991
33333333333333331111111111000001111000000000011100100010100000011110111111111111000000000000000001999889888888892988888889889991
33333300003333331111111111000001111111111111111110011001111111111101111111111111000000000000000001999888999998892988999998889991
33333011110333331111111110000000111111111111111111111111111111111011111111111111000000000000000001999888888889892989888888889991
33330111111033331111111110000000111111111111111111111111111111111011111111111111000000000000000001999888888888892988888888889991
33301111111103331111111111000001111111111111111111111111111111110111111111111111000000000000000001999999999999992999999999999991
33301111111103331111111111110111111111111111111111111111111111110111111111111111000000000000000001111111111111111111111111111111
e1e1e1e1e1e1e1250000000000000095e1e1e1e1e1e1e1e1000000a69797979737979797b6000000000000000000000000000070000000002c00000000000000
00000000003c00000000005e00008e00000000deee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e1e1e1e1e1e1e1350000000000000085e1e1e1e1e1e1e1e100001697473797679797675797060000000000000000000000000060000000002e3d3d3d3d3d3d3d
3d3d3d3d3d3e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e1e1e1e1e1e1e1e125000000000095e1e1e1e1e1e1e1e1e100959797979797979797979737972500000000000000003040005000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e1e1e1e1e1e1e1e135000000000085e1e1e1e1e1e1e1e1e100853797979797979797979797373500000000100020000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e1e1e1e1e1e1e1e1e1e1e1e1a7e1e1e1e1e1e1e1e1e1e1e1a63797979797979797979797979797b6000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01111111000000111110000001111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01999999100001999991000019999991000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00199999911119999999111199999910000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00199989999999992999999999899910000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00199889988899892989988899889910000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00199898888889892989888888989910000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01199898899889892989889988989910000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01999889988889892989888899889991000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
044000000000044004000000000000400000000000001cc1000000000000000001c100000000000000000000001c10000000000000000001c100000000000000
4004004444004004040000000000004000000000000011c1000000000000000001c100000000000000000000000110000000000000000001c100000000000000
4044040000404404010000000000001000000000000001c100000000000000001c100000000000000000000000000000000000000000001c1000000000000000
4000040000400004044440000004444000000000000001c1000000000000000001000000000000000000000000000000000000000000001c1000000000000000
0444400000044440400004000040000400000000000001c10000000000000000000000000000000000000000000000000000000000000001c100000000000000
010000000000001040440400004044040000000000001c100000000000000000000000000000000000000000000000000000000000000001c100000000000000
04000000000000404004004444004004000000000001cc1000000000000000000000000000000000000000000000000000000000000000001100000000000000
04000000000000400440000000000440000000000000110000000000000000000000000000000000000000000000000000000000000000000000000000000000
60000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000666666600
66660000000000006666666660000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000666666666666
66666660000000666666666666600000000000000000000000000000000000000000000000000000000000000000000000000000000000000666666666666666
66666666600006666666666666660000000000000000000000000000000666000000000000000000000000000000000000000000000000066666666666666666
66666666666066666666666666666000000000000000000000000006666666660000000000000000000000000006666666660000000006666666666666666666
66666666666666666666666666666666600000000666660000000666666666666600000000666666666000000666666666666600000066666666666666666666
66666666666666666666666666666666666000066666666600006666666666666660000666666666666660066666666666666660000666666666666666666666
66666666666666666666666666666666666660666666666666066666666666666666066666666666666666666666666666666666606666666666666666666666
__label__
11111000000001000000001000100000000000100010000000000010001000000000001000100000000000100010000000000010001000000100000000111110
10000101001101000000100100101100000010010010110000001001001011000000100100101100000010010010110000001001001011000101100101000010
10c6000101001000100001010001001010000101000100101000010100010010100001010001001010000101000100101000010100010010001001010006c010
101c00100000011000111100011000000011110001100000001111000110000000111100011000000011110001100000001111000110000011000000100c1010
10001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100010
01000000000000000000000000000000000000000000000000000000010000cccc00001000000000000000000000000000000000000000000000000000000100
0001000000000000000000000000000000000000000000000000000000100cccccc0010000000000000000000000000000000000000000000000000000010000
01100000000000000000000000000000000000000000000000000000100000010000000100000000000000000000000000000000000000000000000000001100
0000000000000000000000000000000000000000000000000000000010000000c000000100000000000000000000000000000000000000000000000000000000
0010000000000000000000000000000000000000000000000000000000100000c000010000000000000000000000000000000000000000000000000000001000
010000000000000000000000000000000000000000000000000000000100000c0000001000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000010000000000001000000000000000000000000000000000000000000000000000000100
00100000000000000000000000000000000000000000000000000000001000000000010000000000000000000000000000000000000000000000000000001000
1101000000000000000000000000000000000000000000000000000011c1001001001c1100000000000000000000000000000000000000000000000000010110
00010000000000000000000000000000000000000000000000000000001101011010110000000000000000000000000000000000000000000000000000010000
00000000000000000000000000000000000000000000000000000000001000000000010000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
11010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001011
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
00110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001100
01010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
11010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001011
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110
10000000000000000000000000000000000000055500000555000005550000004000000040000000000000cccc00000000000000000000000000000000000001
001100000000000000000000000000000000005766500057665000576650000000000000000000ccc0000cccccc0000000000000000000000000000000001100
01010000000000000000000000000000000000566650005666500056665000000400000004000ccccc0000010000000000000000000000000000000000001010
0001000000000000000000000000000000000566510005665100056651000000000000000000000100000000c000000000000000000000000000000000001000
0001000000000000000000000000000000000566665005666650056666500400000004000000000c00000000c000000000000000000000000000000000001000
0000000000000000000000000000000000000055510000555100005551000000000000000000000c0000000c0000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00000000000000000000000000000000000000000000009999000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000055500094994900000000000000000000040002222000000004000000000000000000000000000000000000100
01000000000000000000000000000000000000576650944994490000000000000000000000000222220000000000000000000000000000000000000000000010
01000000000000000000000000000000000000566650944994490000b0300000b030000004000022222000000400000000000000000000000000000000000010
0010000000000000000000000000000000000566510094a99a49000003000000030000000000000d22d000000000000000000000000000000000000000000100
11010000000000000000000000000000000005666650942992490b0300000b0300000400000000000dd004000000000000000000000000000000000000001011
0001000000000000000000000000000000000055510094499449003000000030000000000000000d220000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000999999990000000000000000000000000000010000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110
10000000000000000000000000000000000000055500000000000000000000000000000000000000000000cccc00000000000000000000000000000000000001
001100000000000000000000000000000000005766500000000000000000000000000000000000ccc0000cccccc0000000000000000000000000000000001100
010100000000000000000000000000000000005666500000b0300000b0300000b0300000b0300ccccc0000010000000000000000000000000000000000001010
0001000000000000000000000000000000000566510000000300000003000000030000000300000100000000c000000000000000000000000000000000001000
000100000000000000000000000000000000056666500b0300000b0300000b0300000b030000000c00000000c000000000000000000000000000000000001000
0000000000000000000000000000000000000055510000300000003000000030000000300000000c0000000c0000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000055500000000000000000000000000000000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000576650000000000000000000000000000000000000000000ccc000000000000000000000000000000000000010
010000000000000000000000000000000000005666500000b0300000b0300000b0300000b0300000b0300ccccc00000000000000000000000000000000000010
00100000000000000000000000000000000005665100000003000000030000000300000003000000030000010000000000000000000000000000000000000100
110100000000000000000000000000000000056666500b0300000b0300000b0300000b0300000b030000000c0000000000000000000000000000000000001011
000100000000000000000000000000000000005551000030000000300000003000000030000000300000000c0000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110
10000000000000000000000000000000000000055500000555000005550000055500000000000000000000000000000000000000000000000000000000000001
00110000000000000000000000000000000000576650005766500057665000576650000000000000000000000000000000000000000000000000000000001100
010100000000000000000000000000000000005666500056665000566650005666500000b0300000b0300000b030000000000000000000000000000000001010
00010000000000000000000000000000000005665100056651000566510005665100000003000000030000000300000000000000000000000000000000001000
000100000000000000000000000000000000056666500566665005666650056666500b0300000b0300000b030000000000000000000000000000000000001000
00000000000000000000000000000000000000555100005551000055510000555100003000000030000000300000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000cccc0000cccc00000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000000000000000000000000cccccc00cccccc0000000000000000000000000000000000010
0100000000000000000000000000000000000000b0300000b0300000b0300000b0300000b0300001000000010000000000000000000000000000000000000010
00100000000000000000000000000000000000000300000003000000030000000300000003000000c0000000c000000000000000000000000000000000000100
1101000000000000000000000000000000000b0300000b0300000b0300000b0300000b0300000000c0000000c000000000000000000000000000000000001011
0001000000000000000000000000000000000030000000300000003000000030000000300000000c0000000c0000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
00110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001100
01010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
11010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001011
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
00110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001100
01010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000
11010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010110
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001100
00010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100
10001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100010
101c00100000011000111100011000000011110001100000001111000110000000111100011000000011110001100000001111000110000011000000100c1010
10c6000101001000100001010001001010000101000100101000010100010010100001010001001010000101000100101000010100010010001001010006c010
10000101001101000000100100101100000010010010110000001001001011000000100100101100000010010010110000001001001011000101100101000010
11111000000001000000001000100000000000100010000000000010001000000000001000100000000000100010000000000010001000000100000000111110
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__map__
000000001c1d131c1d0000001c1d1a1a1a0013000505051c1d00130000000001010102d0d100001c1d001c1d13000000001300000014000000000000c140020213001c1d0000000000130000000000001c1d000000000000000000d0d1c0c11302101400021002020213d0d1000c0c0c021c1d00000000000000000000000000
000000c12c2d022c2dd0d1002c2d1ac01a0002000505052c2d02020200001001010102c11300002c2d002c2d02000000000202020202000000000001013e450202c02c2d04001c1d022a2525252102002c2d3e3f02022a25252600020202c002020202000702060202021c1d000c0c0c022c2d13000000000000000000000000
1c1dc002020202020202c1001a1a1ac11a0002000202021c1d0202020007020202022b0202000001010101010200000000d0d1c0c1c000151515000101463f0202c1020202002c2d02020101010202000213463f0202010101240002c002c1c02b1515000202020202022c2d0002020202020202000000000000000000000000
2c2d10020201010102020200021c1d001a0002001002042c2d040202000001010101011c1d000202c0d0d1c10200040000000000000000151515000102024002d0d1020202001c1d02020101010202000202c0400202010101240002020201c10215150000001c1d00000000001c1d020202022a000000000000000000000000
070202020101010101020200022c2d00021002000202021c1d020202000000010101012c2d000202021c1d020200021c1d1c1d1c1d000004021400020202464b4502020505002c2d02020202020202000202c14002020101012200c020c101c00215150000022c2d01c00213002c2d0202020224000000000000000000000000
00c00202020101010202020007000000020202000202022c2d01010100000000000000000000d0d1022c2d021c1d2b2c2d2c2d2c2d00000202020002020202463f0205050500001c1dd0d102c10000000202443fc12b0202020200c124c001c102c1140007020101010c020200010101c1022b22000000000000000000000000
0000c10202020202020200000000000000000000000700000001010100000000000000000000022b1a1a1a2b2c2d0202020505050200000202020004022b0202400205050500002c2d0010020202070002023e3f022b02021c1d00c02a0201020202070002020101010c02020001010102020202000000000000000000000000
00000000000000000000000000000000000000000000000013000000000000000000000000000200001c1d02c10002020205050507000000000000020202024443450205050000000000000000000000d0d13e47c1d0d1072c2d000000000000000000000002010101c102000000000000000700000000000000000000000000
000000000000000000000000000000000000000000022a2525252102000101010101c00000000700002c2dc10000000000000000000000000000001c1d444b4702400202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000020201010102020001c0130601c10000000000000000000000000000000000000000000000002c2d3e47020246451c1d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000001c1d0202010101020200010202020120000000000000000000000000000000000000000000000000444b3f1c1d0202402c2d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000002c2d02020202020202000101010101240000000000000000000000000000000000000000000000004343472c2d0207464b450000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000d0d107c1000000c1020702c12a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
05051401c0140104000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
05130101c02a21020000000000000000000002022c2d01c00202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
05020401c10202200000000000000000000007020101010c0213020000000000000000000000000000c0022325252a010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
022b02c1444b45240000000000000000000002020101010c020202000000000000000000000000000040024449132b010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
072b2b2b4643472a000000000000000000000202010101c10202020000020220c0020207000014040002024802022b060700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
060202232a1515c0000000000000000000000000000000000000000000050624020202022b020202004a450202444b450600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000005c124020202021c1d010100024642424343470600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000005022ac10202022c2dc00100020202020202020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000202c01515c0041c1d010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000014c11515c102022c2d021300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000020202c00201010000020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
6000000000000000000000000000000000000000000061000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1e51000000000000000000000000000000000000005a1e1e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b0b1b2b300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1e1e5100000000000000000000000000000000005a1e1e1e0000000000006372726200000000000000000000000000000000000000000000000000000000000000000000000000000000000000005c5d5e5f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1e1e1e54550000000000000000000000000056571e1e1e1e0000000000005879795300000000000000000000000000000000000000000000000000000000000000000000000000000000000000006c6d6e6f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1e1e1e1e1e600000000000000000000000611e1e1e1e1e1e0000000000617479767560000000000000000000000000000000000000000000000000000000000000000000000000000000000000007c7d7e7f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1e1e1e1e1e1e52000000000000000000591e1e1e1e1e1e1e0000000059797979797979520000000000000000000d00000000000000000000e0d2d2d2d2d2d2d2d2d2d2d2d2e100000000c4c5c6c7c8c9cacbcccdcecf000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1e1e1e1e1e1e53000000000000000000581e1e1e1e1e1e1e0000000058797973797979530000000000000000000c0b0a0900080000000000c2000000000000000000000000c300000000d4d5d6d7d8d9dadbdcdddedf000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000400001074519501077310a7250d700017000a7050a7050a7050a7050970508705087050770507705067050670306703067030570304703037030270301703007030570304703047030c0030c0030c00300000
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
010200200013400100001350015300175000000013400135001340003500105000340013000150000700006500154000500005500105000530003300130001350013400151000510013500000000350015400155
010200200053403555035740355500535000000053400555000740006500055000000053400555005450000000034000550004418025000000054400555005740017300000005540057100555000000005300000
00020000177102d0100d0103271025510181101f5101b1103351017110215101d11132511231102a5101d1102b5102511010110387101d7100271033700377003d70033700367002370027700077000b70000000
00040000165751e7750a5740757513774045750257406775067750157501574015750b77400575005640576500164007650275400555000540054500544017350052500515015040150501504015050150401500
000200000074400141007410010100051000010015100101000410000100741001310073500720001150b6050d6000f600106001160011600116001160011600106000f6000b6000a60003000050000300007000
000100000b3000560003510046000360005510057100151037010295100560003510067102a0101e51005600005100701038010297100860031010225100d5100351005600310202351004510035101250037500
01300000006050c003006050c603006050c003006050c6030c603006050c0030c603006050c003006050c003006050c003006050c603006050c003006050c6030c603006050c0030c603006050c003006050c003
01300000000000000004000040000100000000070050c00500005000050c0050c00501005000050c0050c005000050000504005040050100500005070050c0050000500005000030100505000051001050011500
01300000050050500500002010020500505005145051150510505130050c00500000240051c50518005110050d0050d0050f005180050c0051600514004130021000510005185051800510505100052450530005
0118000007000070050700007005020000200103001030050700007005070000700516505165051350513505125001250515000150050e0000e001020010200526000260051e5001e5051a0001a0051300013005
01300000050050500500002010020500505005145051150510505130050c00500000240051c50518005110050d0050d0050f005180050c00516005140041300210005100051850500000105051f0052250514105
0118000000000000010c0050c0010c005000000c0000c0010100001001000050000100005000000c00500000130051200511505100050f50500000075050000008505030050b505050050e00509005135050e005
01180000186050c6050c0033f205006010060100605000000c60118605005030010318605000000c603000000c6000c60100605000000c00300601006010060500605000000c0030c10300605000000c00300605
011800001800524001240052400524005000002f705307053100525005190050d00530000300050c00500000300052400518005005053b7052f705237050b5052e00522005160050a00507504135011f5011f505
0118000002050020450205002035065500654506050060350305003045020500203509550095450e0500e035020500204502050020350e5540e5410e0510e035030500304502050020351a5341a5310e0510e055
0118000002050020450205002035065500654506050060350305003045020500203509550095450e0500e0350205002045020500203503550035450e0500e0450505005035125501255503050030251155211532
0118000002050020450205002035065500654506050060350305003045020500203509550095450e0500e035020500204502050020450202300053030550200007050070450715007145125351f5350653513555
011800000f0400f0350f0400f03511040110351a0401a0350e0400e035180201802516044160351504215032120401203512040120351a5401a5351a0401a0351254012535120401203526530265353201032015
01180000265142551126511265121a5210e5210e5220e5220e5220e5220e5220d5210c5210b5210a5210952109522095220952209522095220952209522095220852208522085210852500000000000000000000
011800001052410522105221052210522105221052210521045210452204522045250000000000000000000015524155221552215522155221552215522155210952109522095220952509505000000000000000
011800000f0400f0350f0400f03511040110351a0401a0350e0400e035180201802516044160351504215032120401203512040120351a5401a53502000000001254012535210302102524520245151612016125
01300000050550505500062010420506505055145351155510555130350c05500000240351c52518055110250d0550d0550f055180550c05516015140541305210055100551855500000105551f0452253514125
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
__music__
00 0a0b4344
00 43444344
00 47094344
00 48494344
01 4c424344
01 4d424344
02 4e424344
01 50424344
02 51424344
00 53544344
01 292a4344
00 2b2a5a44
00 2e2a4344
00 2c2a4344
00 2f304344
00 31304344
00 2f304344
00 31304344
00 1e304344
00 1f304344
00 1e304344
00 20304344
00 2d304344
00 24304344
00 2d304344
00 21304344
00 30424344
02 30424344

