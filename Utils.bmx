Function LoadProps:TMap(filename:String)
    Local config:TStream = ReadStream(filename)
    if not config then return Null
    
    Local map:TMap = CreateMap()
    Local s$, key$, value$
    Local sa$[]
    
    While not Eof(config)
	s = ReadLine(config).Trim()
	if s.length = 0 OR s[0..1] = "#" then Continue
	sa = s.split("=")
	key = sa[0].Trim()
	value = sa[1].Trim()
	MapInsert(map, key, value)
    Wend
    CloseStream(config)
    return map
End Function
