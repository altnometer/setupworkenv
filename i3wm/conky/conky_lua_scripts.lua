function conky_pad_num(format, conky_cmd )
    --return string.format( '%03i', conky_parse( conky_cmd ) )
    if conky_cmd then
        num = conky_parse( conky_cmd )
        if ( num == nil or num == '') then
            return ''
        else
            --num_int = tonumber(num)
            return string.format( format,  num )
        end
    else
        return ''
    end
end

--function pad(s, width, padder)
function conky_pad_try(conky_cmd)
    if conky_cmd then
        s = conky_parse(conky_cmd)
        s = string.gsub(s, ' ', '')
        --width = width or -4
        width =  -8
        padder = "___________"
        padder = string.rep(padder or "_", math.abs(width))
        --print ( '****************************')
        --print (widht)
        --print (padder)
        --print ( '****************************')
        --padder = strring.rep("_", abs(width))
        --return string.sub(padder .. s, 2)
        --return strlen(s)
        --return string.format( '%0i', string.strlen(s))
        if width < 0 then return string.sub(padder .. s, width) end
        return string.sub(s .. padder, 1, width)
    else
        return ''
    end
end
