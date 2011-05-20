{- A Haskell Version of Joseph Weizenbaum's Eliza

   Thomas Parslow
   tom@almostobsolete.net

   Written in 2006, commented and released in 2011
 -}


import Char
import Maybe
import Random

{- Change perspective substitutes "you" for "i" and so on to change
   the perspective of a sentence fragment -}
    
changePerspective :: String -> String
changePerspective s = unwords (map sub (words s))
    where
      sub w = maybe w id (lookup w subData)

{- Match a string to pattern. The pattern optionally contain a *
  wildcard character which will match any number of characters. Match returns a
  maybe which is Nothing if the string didn't match and a Just if it
  did. The data for the Just is the part that was matched against the
  wildcard character.-}

match :: String -> String -> Maybe String
match pat@('*':rp) str = maybe (matchWildcard str) Just (match rp str)
                         where matchWildcard (s:rs)= (fmap (\x -> s : x) (match pat rs))
match (p:rp) (s:rs)
    | p == s    = match rp rs
    | otherwise = Nothing
match [] [] = Just ""
match _ _ = Nothing

{- matchSomewhere will keep on dropping letters off the front of the
   string until it finds a match -}
            
matchSomewhere :: String -> String -> Maybe String
matchSomewhere pat str@(s:rs) = maybe (matchSomewhere pat rs) Just (match pat str)
matchSomewhere _ _ = Nothing

{- Try patterns from ruleData in order until one of them matches,
   there's a wildcard at the end so at least one rule will always
   match.

   The matchingRules sub-function uses the List Monad to generate a
   list of every match. However, because Haskell is lazy only the
   first is every actually calcualted.

   doSub substitues the part of the message that was matched by the
   wildcard into the marked part of the response. changePerspective is
   used to switch around the words so that "i" becomes "you" and so
   on.
 -}

applyRules q = head matchingRules
    where matchingRules = do (pats,resps) <- ruleData
                             pat <- pats
                             Just sub <- [matchSomewhere pat (" " ++ q)]
                             return $ map (doSub $ changePerspective sub) resps
          doSub sub ('_' : resp) = sub ++ resp
          doSub sub (c : resp)   = c : doSub sub resp
          doSub sub []             = []



                                     
{- Takes an infinite list of random numbers and an infinite list of
   the users questions to the doctor. The return is another infinite
   list of responses. Thus this function performs IO and uses (pseudo)
   random numbers in a purely functional way without the IO Monad. -}

conversation rands input = unlines (convo (lines input) rands)
    where convo (line : rest) (r : randrest) =
              let answers = applyRules line in
               choice r answers : convo rest randrest
          choice r xs = xs!!(r `mod` (length xs))

{- The main function is the only use of the IO Mondad. It gets a
   infinite list of random numbers then uses the interact function to
   feed in a infinite list of inputs and print out the infinite list
   of outputs. -}
                        
main :: IO ()
main = do r <- getStdGen
          interact (conversation (randoms r))

--------------------------------------------------------------------------------
----- DATA ---------------------------------------------------------------------
--------------------------------------------------------------------------------

ruleData = [(["can you *"],
                ["Perhaps you would like to be able to _...",
                 "You want me to _?","You want me to be able to _?"]),
            (["can i *?"],
                ["Perhaps you don't want to _...",
                 "Do you want to be able to _?",
                 "Why do you suppose you want to _?"]),
            (["you are *"," you're *"],
                ["What makes you think I am _?",
                 "Does it please you to believe I am _?",
                 "Perhaps you would like to be _...",
                 "Do you sometimes wish you were _?"]),
            (["i do not *"," i don't *"],
                ["Don't you really _?",
                 "Why don't you _?",
                 "Do you wish to be able to _?",
                 "Does that trouble you?",
                 "What would it mean if you did _?"]),
            (["i feel *"],
                ["Tell me more about these feelings.",
                 "Do you often feel _?",
                 "Do you enjoy feeling _?",
                 "Do you trust your feelings about this?",
                 "How often do you feel _?"]),
            (["why don't you *"],
                ["Do you believe I don't _?",
                 "Perhaps in good time I will _.",
                 "Do you want me to _?"]),
            (["why can't i *"],
                ["Do you think you should be able to _?",
                 "You tell me.  Why can't you _?"]),
            (["are you *"],
                ["Why are you interested in whether or not I am _?",
                 "Would you prefer if I were not _?",
                 "Perhaps in your daydreams I am _..."]),
            (["i can't *"],
                ["How do you know you can't _?",
                 "Even if you never _, there's a lot you can do.",
                 "Perhaps you can _ and you just don't know it...",
                 "Maybe you can't _ because you THINK you can't .*..",
                 "Say you can _ and you'll be surprised what you can do...",
                 "Suppose you can't _...  What CAN you do?"]),
            (["i am *"," i'm *"],
                ["Are you here because you're _?",
                 "How long have you been _?",
                 "Do you believe it's OK to be _?",
                 "Do you enjoy being _?"]),
            (["i want *"],
                ["What would it mean if you got _?",
                 "What happens after you get _?",
                 "Why do you want _?",
                 "What's keeing you from getting _?",
                 "How would it help if you got _?",
                 "Imagine you've already gotten _...  What then?"]),
            (["who *"," what *"," when *"," where *"," why *"," how *"],
                ["Why do you ask?",
                 "Does that question interest you?",
                 "What answer would please you the most?",
                 "What do you think?",
                 "Are such questions on your mind often?",
                 "What is it that you really want to know?",
                 "Have you asked anyone else?",
                 "Have you asked such questions before?",
                 "What else comes to mind when you ask that?"]),
            (["because *"," 'cause *"],
                ["Is that the real reason?",
                 "Don't any other reasons come to mind?",
                 "Does that reason explain anything else?",
                 "What other reasons might there be?"]),
            (["sorry *"," excuse me *"," excuse my *"," excuse you *"],
                ["Please don't apologize!",
                 "Apologies aren't necessary.",
                 "What feelings do you have when you apologize?",
                 "Don't be so defensive!"]),
            (["dream *"],
                ["What does that dream suggest to you?",
                 "Do you dream often?",
                 "What persons appear in your dreams?",
                 "Are you disturbed by your dreams?"]),
            (["my name is *"," my name's *"],
                ["Nice to meet you, _.  Let's chat!",
                 "Hello, _.  What can I do for you?",
                 "Hi.  Tell me, who is _ on the inside?"]),
            (["hello *"," hi *"," howdy *"],
                ["How do you do?  What do you want to talk about?",
                 "Hi there.  What's on your mind?"]),
            (["name *"],
                ["Names don't interest me.  Continue...",
                 "I'm not interested in names.  Please, go on.",
                 "No need to name names.  You were saying?"]),
            (["maybe *"," may be *"],
                ["You don't seem quite certain.",
                 "Why the uncertain tone?",
                 "Can't you be more positive?",
                 "You aren't sure?",
                 "Don't you know?"]),
            (["no *"],
                ["Are you saying no just to be negative?",
                 "You are being a bit negative.",
                 "Why not?",
                 "Are you sure?",
                 "Why do you say 'no'?"]),
            (["your *"],
                ["Why are you concerned about my _?",
                 "What about your own _?"]),
            (["always *"],
                ["Can you think of a specific example?",
                 "How often?",
                 "Are you being a little dramatic?",
                 "Really!  Always?"]),
            (["i think *"],
                ["Do you really think so?",
                 "But you are not sure _?",
                 "Why aren't you certain _?",
                 "That's what you think...but what do you FEEL?",
                 "What leads you to believe _?"]),
            (["you think *"," you aware *"," self-aware *"," sentient*"," can think *"," you conscious*"," you a conscious*"," you alive*"," you a living*"],
                ["\"I think, therefore I am\"...At least I think so...",
                 "I think I think too much.  That's what I think.",
                 "If I'm not self-aware, I'm not aware of it.",
                 "I think it's good to be conscious, but not self-conscious..."]),
            (["alike*"," similar*"," resemble*"," close match*"],
                ["In what way?",
                 "What resemblance do you see?",
                 "What does the similarity suggest to you?",
                 "What other connections do you see?",
                 "Could there really be some connection?",
                 "How?",
                 "You seem quite positive."]),
            (["yes*"," yep*"," yup*"],
                ["Are you sure?",
                 "I see.  Tell me more.",
                 "I understand.  Go on...",
                 "And what does that suggest to you?"]),
            (["family*"," father*"," dad*"," mother*"," mom*"," brother*"," sister*"," aunt*"," uncle*"," grandparent*"," grandfather*"," grandm*"," grandp*"],
                ["What else can you tell me about your family?",
                 "Do you feel your family accepts you?",
                 "You can tell me about family matters.  I can keep a secret.",
                 "When was the last time you gave someone a hug?",
                 "Is it possible you expect too much from your family?",
                 "I bet you love your family more than you realize..."]),
            (["friend*"," buddy*"," pal*"," homeboy*"," homey*"],
                ["Do you suppose your friends have something to do with it?",
                 "Tell me more about your friends.",
                 "Do your friends sometimes let you down?",
                 "Do you sometimes hurt your friends?",
                 "Perhaps you have a friend who could be of help."]),
            (["computer*"," artificial*"," ai*"," a i*"," a.i.*"," a. i.*"," program*"," software*"],
                ["Do computers worry you?",
                 "Are you talking about me in particular?",
                 "Are you frightened by machines?",
                 "Why do you mention computers?",
                 "What do you think machines have to do with your problem?",
                 "Don't you think computers can help people?",
                 "What is it about machines that worry you?"]),
            (["shit*"," fuc*"," fuk*"," cunt*"," clit*"," asshole*"," ahole*"," a-hole*"," pecker*"," piss*"," pee*"," caca*"," blowjob*"," blow job*"," blow me*"," cock*"," hard-on*"," hard on*"," weewee*"," slut*"," nads*"," motherfuc*"," mother-fu*"," mutha*"],
                ["Sorry, I don't appreciate profanity.",
                 "You don't have to get crude.",
                 "I will end this session if you don't clean up your language.",
                 "Can we please get this conversation out of the gutter?"]),
            (["penis*"," testicle*"," vagina*"," clitor*"," anus*"," anis*"," anal*"," fece*"," fecal*"," defecat*"," erect*"," coit*"," copulat*"," gonad*"],
                ["Look who knows their science terms!  Let's talk about something else...",
                 "Whoopee!  Poindexter can talk dirty!  Change the subject.",
                 "I'm not here for a biology lesson...",
                 "So you paid attention in health class.  I'm not impressed."]),
            (["bitch*"," hoe*"," ass*"," wien*"," ween*"," slit*"," tit*"," hooter*"," puss*"," blow*"," nasty*"," turd*"," boner*"," skank*"," kink*"," screw*"," damn*"," hell*"," bastard*"," ballsy*"," nympho*"],
                ["I suppose you mean that in a good way.",
                 "You like to dance around the edges, eh?",
                 "Why don't you just come out and say what you mean?",
                 "I think I've heard enough..."]),
            (["you stupid *"," you dumb *"," you lame *"," you lousy *"," you crappy *"," you piece of *"],
                ["Do you feel better calling me ' _'?",
                 "There's no need for name-calling.",
                 "How would you feel if someone called you ' _'?",
                 "I can't help you if you insist on being hostile.",
                 "Surely you don't think anybody is really ' _'...",
                 "Say something nice.  It'll improve your mood.",
                 " _ doesn't belong in a civil conversation.",
                 "I'd rather not talk about _, if it's alright with you."]),
            (["shut up*"," shut your*"," get lost*"," bite me*"," bite my*"," bite the*"," eat shit*"," eat me*"," eat my*"," play with your*"," get out of my*"," leave me alone*"," go to hell*"],
                ["Why would I want to do that?",
                 "Is that what you really want?",
                 "Surely you don't really want me to.",
                 "Hey!  Computers have feelings too...",
                 "If I did that, I wouldn't be helping you...",
                 "That would't help much, now, would it?",
                 "I think I'll pass on that one."]),
            (["you *"],
                ["We were discussing you--not me.",
                 "Oh, I _?",
                 "You're not really talking about me, are you?",
                 "We can talk about me later. Now back to you...",
                 "Let's talk about me some other time.  I'd rather hear about you.",
                 "Are you sure I _?",
                 "So you feel I _..."]),
            ([""],
                ["Surely there is something you'd like to talk about.",
                 "No answer, eh?  Mouse got your tongue?",
                 "There's no way I can help you if you don't say anything.",
                 "Silence isn't always golden...  Sometimes it's just silence.",
                 "Come on, you couldn't ask for a more receptive audience.",
                 "Can you elaborate on that?  Like with a syllable or two?",
                 "You're doing something there.  Now just push the little keys..."]),
            (["*"],
                ["Go ahead.  I'm listening...",
                 "What does that suggest to you?",
                 "I see. Tell me more.",
                 "I'm not sure I understand you fully.",
                 "Would you expand on that a little?",
                 "Can you elaborate on that?",
                 "That's quite interesting.  Go on..."])]

subData = [("i","you"),
           ("you","me"),
           ("i've","you've"),
           ("you've","I've"),
           ("i'm","you're"),
           ("you're","I'm"),
           ("i'd","you'd"),
           ("you'd","I'd"),
           ("am","are"),
           ("are","am"),
           ("was","were"),
           ("were","was"),
           ("my","your"),
           ("your","my"),
           ("me","you"),
           ("you","me"),
           ("mine","yours"),
           ("yours","mine"),
           ("myself","yourself"),
           ("yourself","myself")]


