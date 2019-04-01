#####################
### Rally Shot Areas  
#####################
p1_ad = function(){rect(60,0,120,0.3*30,lty = 3, border ="gray", col = "deepskyblue")}
p1_ad_deep = function(){rect(105,0,120,0.3*30, border ="gray", col = "deepskyblue")}
p1_ad_mid = function(){rect(90,0,105,0.3*30, border ="gray", col = "deepskyblue")}
p1_ad_short = function(){rect(60,0,90,0.3*30, border ="gray", col = "deepskyblue")}

p1_mid = function(){rect(60,0.3*30,120,0.7*30, border ="gray", col = "deepskyblue")}
p1_mid_deep =  function(){rect(105,0.3*30,120,0.7*30, border ="gray", col = "deepskyblue")}
p1_mid_mid =   function(){rect(90, 0.3*30,105,0.7*30, border ="gray", col = "deepskyblue")}
p1_mid_short = function(){rect(60, 0.3*30,90,0.7*30, border ="gray", col = "deepskyblue")}

p1_deuce = function(){rect(60,0.7*30,120,30, border ="gray", col = "deepskyblue")}
p1_deuce_deep =  function(){rect(105,0.7*30,120,30, border ="gray", col = "deepskyblue")}
p1_deuce_mid = function(){rect(90,0.7*30,   105,30, border ="gray", col = "deepskyblue")}
p1_deuce_short = function(){rect(60,0.7*30,90,30, border ="gray", col = "deepskyblue")}


p2_deuce = function(){rect(0,0,60,0.3*30,lty = 3, border ="gray", col = "deepskyblue")}
p2_deuce_deep = function(){rect(0,0,15,0.3*30, border ="gray", col = "deepskyblue")}
p2_deuce_mid = function(){rect(15,0,30,0.3*30, border ="gray", col = "deepskyblue")}
p2_deuce_short = function(){rect(30,0,60,0.3*30, border ="gray", col = "deepskyblue")}

p2_mid = function(){rect(0,0.3*30,60,0.7*30, border ="gray", col = "deepskyblue")}
p2_mid_deep =  function(){rect(0,0.3*30,15,0.7*30, border ="gray", col = "deepskyblue")}
p2_mid_mid =   function(){rect(15, 0.3*30,30,0.7*30, border ="gray", col = "deepskyblue")}
p2_mid_short = function(){rect(30, 0.3*30,60,0.7*30, border ="gray", col = "deepskyblue")}

p2_ad = function(){rect(0,0.7*30,60,30, border ="gray", col = "deepskyblue")}
p2_ad_deep =  function(){rect(0,0.7*30,15,30, border ="gray", col = "deepskyblue")}
p2_ad_mid = function(){rect(15,0.7*30,   30,30, border ="gray", col = "deepskyblue")}
p2_ad_short = function(){rect(30,0.7*30,60,30, border ="gray", col = "deepskyblue")}

#####################
## Serve Location ###
#####################
serve_deuce_wide = function(){rect(60,25,90,30, border ="gray", col = "deepskyblue")}
serve_deuce_body = function(){rect(60,20,90,25, border ="gray", col = "deepskyblue")}
serve_deuce_t = function(){rect(60,15,90,20, border ="gray", col = "deepskyblue")}

serve_ad_wide = function(){rect(60,0,90,5, border ="gray", col = "deepskyblue")}
serve_ad_body = function(){rect(60,5,90,10, border ="gray", col = "deepskyblue")}
serve_ad_t = function(){rect(60,10,90,15, border ="gray", col = "deepskyblue")}


blank_court = function(){
        plot(x=c(0,0,120,120),y = c(0,30,30,0), col = "white", ylim = c(-12,42),xlim = c(-12,130))
        rect(0,0,120,30)
}
lines_court = function(){
        lines(x=c(60,60),y = c(0,30),lty = 2,lwd = 3)
        lines(x=c(30,30),y = c(0,30))
        lines(x=c(90,90),y = c(0,30))
        lines(x=c(30,90),y = c(15,15))
        lines(x=c(0,1),y = c(15,15))
        lines(x=c(119,120),y = c(15,15))
}





rally_end = function(){}
        #e = "unknown"
        #d = "deep"
        #x = "wide and deep"
        #n = "net"
        #w = "wide"
        #! = "shank"
        #* = "winner"
        

        
