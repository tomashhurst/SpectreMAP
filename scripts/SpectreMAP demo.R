

### Demo dataset

    str(spatial.dat, 3)

    as.matrix(names(spatial.dat$`20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac`$RASTERS))

    make.spatial.plot(spatial.dat,
                      image.roi = '20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac',
                      image.channel = 'CD20_Dy161')
