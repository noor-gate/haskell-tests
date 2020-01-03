import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process]
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp i p
  = fromJust $ lookup i p

states :: LTS -> [State]
states t
  = nub (states' (map fst t))
    where
      states' :: [(State, State)] -> [State]
      states' []
        = []
      states' ((c, c') : cs)
        = c : c' : (states' cs)

transitions :: State -> LTS -> [Transition]
transitions s []
  = []
transitions s (t@((a, _), _) : ts)
  | s == a = (t : transitions s ts)
  | otherwise = transitions s ts


alphabet :: LTS -> Alphabet
alphabet
  = nub . map snd

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (Prefix i p)
  = i : actions p
actions (Choice (p:ps))
  = actions p ++ actions (Choice ps)
actions _
  = []

--accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts [] _
  = True
accepts m p
  = accepts' m ((snd . head) p)
    where
      accepts' [] _
        = True
      accepts' m' (Ref r)
        = accepts' m' (lookUp r p)
      accepts' m' (Choice c)
        = or $ map (accepts' m') c
      accepts' (i:is) (Prefix n e)
        | n == i = accepts' is e
        | otherwise = False


------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition
                   -> Alphabet -> Alphabet
                   -> StateMap
                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), i) ((s', t'), i') a1 a2 m
  | i == i' = [((a, b), i)]
  | elem i a2 && elem i' a1 = []
  | elem i' a1 = [((a, c), i)]
  | elem i a2 = [((a, d), i')]
  | otherwise = [((a, c), i), ((a, d), i')]
    where
      a = lookUp (s, s') m
      b = lookUp (t, t') m
      c = lookUp (t, s') m
      d = lookUp (s, t') m

pruneTransitions :: [Transition] -> LTS
pruneTransitions n
  = nub $ visit 0 []
    where
      visit :: State -> [State] -> [Transition]
      visit s v
        | elem s v = []
        | otherwise = t ++ concatMap (flip visit (s : v)) (map (snd . fst) (t))
          where
            t = transitions s n

------------------------------------------------------
-- PART IV

--compose :: LTS -> LTS -> LTS
compose t1 t2
  = (pruneTransitions . concat) [composeTransitions x y (alphabet t1) (alphabet t2) statePr | (x, y) <- cartTr]
    where
      statePr = zip [(x, y) | x <- (states t1), y <- (states t2)]
                [0..length (states t1) * length (states t2) - 1]

      cartTr  = nub [( ((fst x, t), lookUp (fst x, t) t1), ((snd x, t'), lookUp (snd x, t') t2)) | (x, y) <- statePr, t <- (map (snd . fst)
                (transitions (fst x) t1)), t' <- (map (snd . fst)
                (transitions (snd x) t2))]





------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")),
                     Prefix "end" STOP])

maker
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch
  = ("SWITCH", Ref "OFF")

off
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS,
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS
  = [((0,1),"tick"),((1,0),"tock")]

playLTS
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS
  = [((0,1),"make"),((1,0),"ready")]

userLTS
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS
  = [((0,1),"on"),((1,0),"off")]
