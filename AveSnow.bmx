SuperStrict

Import MaxMod2.RtAudio
Import MaxMod2.OGG

Import "Utils.bmx"

SetAudioStreamDriver("MaxMod RtAudio")

Private

'{{{ Type TBackground
Type TBackground
    Field image:TImage
    Field duration:Int
    Field kenburns:Int	' True or False
    Field num_incr:Int, incr:Int
    Field orig_scale_x:Float, orig_scale_y:Float, scale_x:Float, scale_y:Float
    Field burns_scale_incr_x:Float, burns_scale_incr_y:Float
    Field orig_x:Float, orig_y:Float, burns_x:Float, burns_y:Float	' Where to draw the scaled image
    Field burns_x_incr:Float, burns_y_incr:Float  ' How much to increment position each frame.
    
    Global default_flake_alpha_min:Float, default_flake_alpha_max:Float
    Field flake_alpha_min:Float, flake_alpha_max:Float
    
    Method ToString:String() 
	return duration + ", " + kenburns + ", " + orig_scale_x + ", " + orig_scale_y + ", " + scale_x + ", " + scale_y + ", " + burns_scale_incr_x + ", " + burns_scale_incr_y + ", " + burns_x + ", " + burns_y + ", " + burns_x_incr + ", " + burns_y_incr
    End Method
End Type
'}}}

'{{{ Type TFlake
Type TFlake
    Global image:TImage
    Global flake_url:String
    Global min_scale:Float, max_scale:Float
    Global min_alpha:Float, max_alpha:Float
    Global min_speed:Float, max_speed:Float
    Global min_rotation:Float, max_rotation:Float
    Global min_flake_alpha:Float

    Field x#, y#, scale#, speed#, alpha#, rotation#, angle#
    
    Global wind_dir#, wind_speed#, gust_strength#, speed_phase#, phase_increment#
    Global gust_strength_min:Float, gust_strength_max:Float
    Global gust_duration_min:Float, gust_duration_max:Float
    
    Method New()
	scale = Rnd(min_scale, max_scale)
	x = Rand(0, TAveMariaSnow.width)
	y = Rand(0, TAveMariaSnow.height) - TAveMariaSnow.height - 20  ' So that it starts -above- the screen
	speed = Rnd(min_speed, max_speed)
	alpha = Rnd(min_alpha, max_alpha)
	angle = Rnd(0, 359)
	rotation = Rnd(min_rotation, max_rotation)
	if Rand(0, 1) = 1 Then rotation = -rotation
	speed_phase = -1
    End Method
    
    Method UpdatePosition()
	if speed_phase >= 0
	    y :+ sin(wind_dir) * wind_speed * speed
	    x :+ cos(wind_dir) * wind_speed * speed
	endif
	y :+ speed
	if y > TAveMariaSnow.height
	    speed = Rnd(min_speed, max_speed)
	    x = Rand(0, TAveMariaSnow.width)
	    y = -20
	    alpha = Rnd(min_alpha, max_alpha)
	EndIf
	if x > (TAveMariaSnow.width+5)
	    x = -5
	elseif x < -5
	    x = TAveMariaSnow.width + 5
	endif
	angle :+ rotation
	if angle >= 360
	    angle :- 360
	elseif angle < 0
	    angle :+ 360
	endif
    End Method
    
    Function WindGust()
	wind_dir = Rnd(0, 180)
	if wind_dir > 90
	    wind_dir :+ 45
	elseif wind_dir > 45
	    wind_dir :+ 270
	endif
	gust_strength = Rnd(gust_strength_min, gust_strength_max)
	speed_phase = 0
	phase_increment = WindDurationIncrement(gust_duration_min, gust_duration_max)
    End Function
    
    Function WindDurationIncrement:Double(min_t#, max_t#)
	return Rnd(180.0/(TAveMariaSnow.hertz*max_t), 180/(TAveMariaSnow.hertz*min_t))
    End Function
    
    Function UpdateWind()
	if speed_phase >= 0
	    speed_phase :+ phase_increment
	    wind_speed = Sin(speed_phase)*gust_strength
	    if speed_phase > 180 Then speed_phase = -1
	endif
    End Function
    
    Method Draw()
	if y < TAveMariaSnow.height
	    SetScale(scale, scale)
	    Local a:Float = alpha
	    if TAveMariaSnow.state < TAveMariaSnow.STATE_FADEOUT
		if y > (TAveMariaSnow.height - 40)
		    a = a * Max(TAveMariaSnow.height - y - 10, 0)/30.0
		endif
	    endif
	    if TAveMariaSnow.state = TAveMariaSnow.STATE_BEFORE_FADEOUT
		' During the day, we will allow the dimming effect to dim the flakes as much as the background.
		a :* TAveMariaSnow.total_alpha
	    else
		' But not when it's dark.
		a :* Max(TAveMariaSnow.total_alpha, min_flake_alpha)
	    endif
	    SetAlpha(a)
	    SetRotation(angle)
	    DrawImage(image, x, y)
	EndIf
    End Method
    
    Function LoadFlakeImage()
	image = LoadImage(flake_url, MIPMAPPEDIMAGE)
        MidHandleImage(image)
    End Function
End Type
'}}}

'{{{ Type TAveMariaSnow
Type TAveMariaSnow
    '{{{ Globals and fields
    Global width:Int = 800
    Global height:Int = 600
    Const hertz:Int = 60
    Const depth:Int = 32  ' This will be used only in fullscreen mode.
    
    const STATE_BEFORE_FADEIN:Int = 1
    const STATE_FADEIN:Int = 2
    const STATE_BEFORE_FADEOUT:Int = 3
    const STATE_FADEOUT:Int = 4
    const STATE_AFTER_FADEOUT:Int = 5
    
    Global state:Int
    
    Global dim_period_min:Int, dim_period_max:Int
    Global gust_period_min:Int, gust_period_max:Int
    
    Field fullscreen:Int
    
    Field backgrounds:TBackground[]
    Field current_background:Int, next_background:Int, current_background_remaining:Int
    Field background_transitioning:Int
    Field transition_duration:Int
    Field transition_alpha:Float, transition_alpha_incr:Float
    
    Field song_channel:TChannel = Null
    Field song_length:Int  ' In seconds
    Field background_url:String, song_url:String
    Field intro_text:String, intro_duration:Int
    
    Field num_flakes:Int
    Field flakes:TList
    
    Field fadein_start:Int, fadein_duration:Int, fadeout_start:Int, fadeout_duration:Int
    Field fade_alpha:Float, fade_alpha_increment:Float
    
    Global total_alpha:Float   ' Applied to every drawing operation
    '}}}
    
    '{{{ LoadConfig()
    Method LoadConfig:Int()
	Local map:TMap = LoadProps("config.props.txt")
	if not map then return False
	
	Local key$, val$
	Local range:String[]
	
	for key$ = EachIn MapKeys(map)
	    val = String(MapValueForKey(map, key))
	    select key
	    case "fullscreen"
		fullscreen = Int(val)
	    case "resolution"
		range = val.Split(",")
		width = Int(range[0])
		height = Int(range[1])
	    case "background_url"
		background_url = val
	    case "flake_url"
		TFlake.flake_url = val
	    case "song_url"
		song_url = val
	    case "song_length"
		song_length = Int(val)
	    case "fadein_start"
		fadein_start = Int(val)
	    case "fadein_duration"
		fadein_duration = Int(val)
	    case "fadeout_start"
		fadeout_start = Int(val)
	    case "fadeout_duration"
		fadeout_duration = Int(val)
	    case "num_flakes"
		num_flakes = Int(val)
	    case "snowflake_scale"
		range = val.Split(",")
		TFlake.min_scale = Float(range[0])
		TFlake.max_scale = Float(range[1])
	    case "snowflake_alpha"
		range = val.Split(",")
		TBackground.default_flake_alpha_min = Float(range[0])
		TBackground.default_flake_alpha_max = Float(range[1])
	    case "snowflake_speed"
		range = val.Split(",")
		TFlake.min_speed = Float(range[0])
		TFlake.max_speed = Float(range[1])
	    case "snowflake_rotation"
		range = val.Split(",")
		TFlake.min_rotation = Float(range[0])
		TFlake.max_rotation = Float(range[1])
	    case "gust_strength"
		range = val.Split(",")
		TFlake.gust_strength_min = Float(range[0])
		TFlake.gust_strength_max = Float(range[1])
	    case "gust_duration"
		range = val.Split(",")
		TFlake.gust_duration_min = Float(range[0])
		TFlake.gust_duration_max = Float(range[1])
	    case "dimming_period"
		range = val.Split(",")
		dim_period_min = Int(range[0])
		dim_period_max = Int(range[1])
	    case "gusting_period"
		range = val.Split(",")
		gust_period_min = Int(range[0])
		gust_period_max = Int(range[1])
	    case "intro_text"
		intro_text = val
	    case "intro_duration"
		intro_duration = Int(val)
	    case "min_flake_alpha"
		TFlake.min_flake_alpha = Float(val)
	    end select
	Next	' next key in the map
	return True
    End Method
    '}}}
    
    '{{{ LoadBackgrounds()
    Method LoadBackgrounds()
	Local background:TBackground
	if background_url.EndsWith(".txt")
	    Local map:TMap = LoadProps(background_url)
	    Local vals:String[]
	    Local w:Float, h:Float, x:Float, y:Float, final_x:Float, final_y:Float, scale:Float
	    Local zoomout:Int = False
	    
	    Local num_backgrounds:Int = Int(String(MapValueForKey(map, "num_backgrounds")))
	    backgrounds = New TBackground[num_backgrounds]
	    transition_duration = Int(String(MapValueForKey(map, "transition_duration")))
	    transition_alpha_incr = 1.0 / transition_duration / hertz
	    for Local i:Int = 1 to num_backgrounds
		background = New TBackground
		background.image = LoadImage(MapValueForKey(map, "image" + i), FILTEREDIMAGE)
		background.duration = Int(String(MapValueForKey(map, "duration" + i)))
		if MapContains(map, "kenburns" + i)
		    Local duration:Int = background.duration + transition_duration*2
		    background.kenburns = True
		    vals = String(MapValueForKey(map, "kenburns" + i)).Split(",")
		    w = ImageWidth(background.image)
		    h = ImageHeight(background.image)
		    x = Int(vals[0])
		    y = Int(vals[1])
		    scale = Float(vals[2])
		    if scale < 0.0
			zoomout = True
			scale = -scale
		    else
			zoomout = False
		    EndIf
		    background.orig_scale_x = width / w
		    background.orig_scale_y = height / h
		    background.num_incr = duration * hertz
		    background.burns_scale_incr_x = background.orig_scale_x * (scale - 1.0) / background.num_incr
		    background.burns_scale_incr_y = background.orig_scale_y * (scale - 1.0) / background.num_incr
		    ' We must calculation our zooming factors
		    w :* background.orig_scale_x * scale
		    h :* background.orig_scale_y * scale
		    x :* background.orig_scale_x * scale
		    y :* background.orig_scale_y * scale
		    ' Where should we draw the scaled image so that the scaled pixel location is
		    ' as close as possible to the center of the screen?
		    final_x = width/2.0 - x
		    If final_x > 0
			final_x = 0
		    ElseIf final_x < width - w
			final_x = width - w
		    EndIf
		    final_y = height/2.0 - y
		    If final_y > 0
			final_y = 0
		    ElseIf final_y < height - h
			final_y = height - h
		    EndIf
		    background.burns_x_incr = final_x / background.num_incr
		    background.burns_y_incr = final_y / background.num_incr
		    if zoomout
			background.orig_scale_x :* scale
			background.orig_scale_y :* scale
			background.burns_scale_incr_x :* -1.0
			background.burns_scale_incr_y :* -1.0
			background.burns_x_incr :* -1.0
                        background.burns_y_incr :* -1.0
                        background.orig_x = final_x
                        background.orig_y = final_y
		    else
                        background.orig_x = 0.0
                        background.orig_y = 0.0
		    End if
		    background.scale_x = background.orig_scale_x
		    background.scale_y = background.orig_scale_y
		    background.burns_x = background.orig_x
		    background.burns_y = background.orig_y
		Else	' the map doesn't contain kenburns
		    background.kenburns = False
		End If	' end if kenburns
		If MapContains(map, "snowflake_alpha" + i)
		    vals = String(MapValueForKey(map, "snowflake_alpha" + i)).Split(",")
		    background.flake_alpha_min = Float(vals[0])
		    background.flake_alpha_max = Float(vals[1])
		Else
		    background.flake_alpha_min = TBackground.default_flake_alpha_min
		    background.flake_alpha_max = TBackground.default_flake_alpha_max
		endif
		backgrounds[i-1] = background
	    Next	'Next background
	Else	' background_url doesn't end with .txt
	    background = New TBackground
	    background.image = LoadImage(background_url, FILTEREDIMAGE)
	    background.duration = song_length + 100
	    background.kenburns = False
	    background.flake_alpha_min = TBackground.default_flake_alpha_min
	    background.flake_alpha_max = TBackground.default_flake_alpha_max
	    
	    backgrounds = New TBackground[1]
	    backgrounds[0] = background
	End If	' background_url ends with .txt
    End Method
    '}}}

    '{{{ DrawBackground()
    Method DrawBackground(n:Int)
	Local bg:TBackground = backgrounds[n]
	if Not bg.kenburns
	    SetScale(1.0, 1.0)
	    DrawImageRect(bg.image, 0, 0, width, height)
	Else
	    SetScale(bg.scale_x, bg.scale_y)
	    DrawImage(bg.image, bg.burns_x, bg.burns_y)
	    if bg.incr < bg.num_incr
		bg.scale_x :+ bg.burns_scale_incr_x
		bg.scale_y :+ bg.burns_scale_incr_y
		bg.burns_x :+ bg.burns_x_incr
		bg.burns_y :+ bg.burns_y_incr
		bg.incr :+ 1
	    end if
	End If
    End Method
    '}}}

    '{{{ InitGraphics()
    Method InitGraphics()
	Local _depth:Int
	if fullscreen
	    _depth = depth
	    HideMouse()
	else
	    _depth = 0
	Endif
	SetGraphicsDriver(GLMax2DDriver())
	Graphics(width, height, _depth, hertz)
	SetBlend(ALPHABLEND)
	SetClsColor(0, 0, 0)
    End Method
    '}}}
    
    '{{{ DrawCenteredText()
    Function DrawCenteredText:Int (y:Int, s:String, scale# = 1.0)
	Local h%, w%, x%
	h = TextHeight(s) * scale
	w = TextWidth(s) * scale
	x = width / 2 - w / 2
	DrawText(s, x, y)
	Return h
    End Function
    '}}}

    '{{{ DrawIntroText()
    Method DrawIntroText()
	SetColor(100, 100, 100)
	DrawCenteredText(260, intro_text)
    End Method
    '}}}
    
    '{{{ LoadResources()
    Method LoadResources()
	For Local i% = 1 To 2
	    Cls
	    DrawIntroText()
	    Flip
	Next
	Local m:Int = MilliSecs()
	TFlake.LoadFlakeImage()
	LoadBackgrounds()
	If song_url <> "none"
	    Local loop:Int = False
	    if song_url.EndsWith("&")
		loop = True
		song_url = song_url[0..song_url.length - 1]
	    EndIf
	    song_channel = CueMusic(song_url, loop)
	EndIf
	flakes = CreateList()
	TFlake.min_alpha = backgrounds[0].flake_alpha_min
	TFlake.max_alpha = backgrounds[0].flake_alpha_max
	For Local i% = 1 To num_flakes
	    flakes.AddLast(New TFlake)
	Next
	Local elapsed:Int = MilliSecs() - m
	If elapsed < intro_duration * 1000 Then Delay(intro_duration*1000 - elapsed)
    End Method
    '}}}
    
    '{{{ AnimationLoop()
    Method AnimationLoop()
	Local dim_alpha:Float = 1.0	' For the temporary dimming effect
	Local seconds_until_dim:Int = -1
	Local dim_cntr:Int = 0	' To keep track of frame-by-frame dimming.
	Local seconds_until_gust:Int = -1
	
	Local second_timer:TTimer	' Counts seconds since the start of the animation loop
	Local timer_val:Int, old_timer_val:Int	' So we check for state transitions only once per second.
	
	if song_channel <> Null Then
	    SetChannelVolume(song_channel, 0.8)
	    ResumeChannel(song_channel)
	EndIf
	
	For Local i# = 1.0 To 0.0 Step (-1.0/hertz)
	    Cls
	    SetAlpha(i)
	    DrawIntroText()
	    Flip
	Next
	SetColor(255, 255, 255)

	second_timer = CreateTimer(1.0)
	timer_val = 0
	old_timer_val = 0
	state = STATE_BEFORE_FADEIN
	
	While (Not KeyHit(KEY_ESCAPE)) AND (timer_val <= song_length) AND (NOT AppTerminate())
	    timer_val = TimerTicks(second_timer)
	    if (timer_val <> old_timer_val)  ' We must check for state transitions
		select state
		case STATE_BEFORE_FADEIN
		    if timer_val = fadein_start
			fade_alpha = 0.0
			fade_alpha_increment = 1.0 / (fadein_duration * hertz)
			state = STATE_FADEIN
			current_background = 0
			current_background_remaining = backgrounds[current_background].duration
			backgrounds[current_background].incr = 0
			background_transitioning = False
		    Endif
		case STATE_FADEIN
		    if timer_val = (fadein_start + fadein_duration)
			fade_alpha = 1.0
			fade_alpha_increment = 0.0
			state = STATE_BEFORE_FADEOUT
			seconds_until_dim = Rand(dim_period_min, dim_period_max)
			seconds_until_gust = Rand(gust_period_min, gust_period_max)
		    EndIf
		case STATE_BEFORE_FADEOUT
		    if timer_val = fadeout_start
			fade_alpha = 1.0
			fade_alpha_increment = -1.0 / (fadeout_duration * hertz)
			state = STATE_FADEOUT
		    Endif
		case STATE_FADEOUT
		    if timer_val = (fadeout_start + fadeout_duration)
			fade_alpha = 0.0
			fade_alpha_increment = 0.0
			state = STATE_AFTER_FADEOUT
		    EndIf
		case STATE_AFTER_FADEOUT
		    ' Nothing for now.
		end select	' end select state
		
		seconds_until_dim :- 1
		if seconds_until_dim = 0
		    dim_cntr = hertz
		    seconds_until_dim = Rand(dim_period_min, dim_period_max)
		endif
		
		seconds_until_gust :- 1
		if seconds_until_gust = 0
		    TFlake.WindGust()
		    seconds_until_gust = Rand(gust_period_min, gust_period_max)
		endif
		
		current_background_remaining :- 1
		if current_background_remaining = 0 And backgrounds.length > 1
		    background_transitioning = True
		    transition_alpha = 1.0
		    next_background = (current_background + 1) Mod backgrounds.length
		    Local nbg:TBackground = backgrounds[next_background]
		    nbg.scale_x = nbg.orig_scale_x
		    nbg.scale_y = nbg.orig_scale_y
		    nbg.burns_x = nbg.orig_x
		    nbg.burns_y = nbg.orig_y
		    nbg.incr = 0
		End if
		old_timer_val = timer_val
	    EndIf	' Timer ticked
	    
	    Cls
	    
	    fade_alpha :+ fade_alpha_increment
	    total_alpha = fade_alpha
	    
	    if dim_cntr >= 0
		dim_alpha = 0.5 + Abs(Float(dim_cntr - hertz/2)/hertz)
		total_alpha :* dim_alpha
		dim_cntr :- 1
	    EndIf
	    
	    If state <> STATE_BEFORE_FADEIN AND state <> STATE_AFTER_FADEOUT
		SetRotation(0)
		if Not background_transitioning
		    SetAlpha(total_alpha)
		    DrawBackground(current_background)
		Else
		    SetAlpha(total_alpha * transition_alpha)
		    DrawBackground(current_background)
		    SetAlpha(total_alpha * (1.0 - transition_alpha))
		    DrawBackground(next_background)
		    transition_alpha :- transition_alpha_incr
		    if transition_alpha <= 0
			background_transitioning = False
			current_background = next_background
			current_background_remaining = backgrounds[current_background].duration
			TFlake.min_alpha = backgrounds[current_background].flake_alpha_min
			TFlake.max_alpha = backgrounds[current_background].flake_alpha_max
		    End If
		End If
	    End If
	    
	    for Local flake:TFlake = EachIn flakes
		flake.UpdatePosition()
		flake.Draw()
	    Next
	    TFlake.UpdateWind()
	    
	    Flip
	Wend	' Main while loop
	
	StopTimer(second_timer)
    End Method
    '}}}
    
    '{{{ Shutdown()
    Method Shutdown()
	if song_channel <> Null Then StopChannel(song_channel)
	EndGraphics()
    End Method
    '}}}
    
    '{{{ Run() and RunApp()
    Method Run()
	if not LoadConfig() then return
	SeedRnd(MilliSecs())
	InitGraphics()
	LoadResources()
	AnimationLoop()
	Shutdown()
    End Method
    
    Function RunApp()
	Local instance:TAveMariaSnow = New TAveMariaSnow
	instance.Run()
    End Function
    '}}}
End Type
'}}}

TAveMariaSnow.RunApp()

' :folding=explicit:collapseFolds=1:
