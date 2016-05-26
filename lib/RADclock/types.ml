type counter = Cstruct.uint64

type stamp = {
    ta:     counter;
    tb:     float;
    te:     float;
    tf:     counter;
}

type quality = NG | OK

type sample = {
    quality:    quality;
    ttl:        int;
    stratum:    Wire.stratum;
    leap:       Wire.leap;
    refid:      Cstruct.uint32;
    rootdelay:  float;
    rootdisp:   float;
    stamp:      stamp;
}


