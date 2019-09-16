#load @".\.paket\load\netcoreapp2.2\main.group.fsx"

module Helpers =
    let binv op x = fun y -> op y x
    let dec = binv (-) 1
    let inc = binv (+) 1
    let roundDice = floor
    let boolToFloat x = if x then 1.0 else 0.0
    let applyTwice f1 f2 x = f1 x, f2 x
    let nextInt mn mx (rand : System.Random) = rand.Next(mn, mx + 1)
    let generator f = fun _ -> f () 
    let (|!>) a b = [a] |> b
module List =
    let wrap x = [x]

type Rank =
    | Number of int
    | Jack
    | Knight
    | Queen
    | King
    | Joker
type Suit =
    | Black
    | Red
    | Diamonds
    | Hearts
    | Clubs
    | Spades
    | Swords
    | Cups
    | Wands
    | Coins
and Card =
    | MinorCard of Suit * Rank
    | SpecialCard of ImageCard
    | SuitedSpecialCard of Suit * ImageCard
and ImageCard =
    | Magician = 1
    | HighPreiestess = 2
    | Empress = 3
    | Emperor = 4
    | Hierophant = 5
    | Lovers = 6
    | Chariot = 7
    | Strength = 8
    | Hermit = 9
    | WheelOfFortune = 10
    | Justice = 11
    | HangedMan = 12
    | Death = 13
    | Temperance = 14
    | Devil = 15
    | Tower = 16
    | Star = 17
    | Moon = 18
    | Sun = 19
    | Judgement = 20
    | World = 21
    | Fool = 0
type DiceNum =
    | D2 = 2
    | D3 = 3
    | D4 = 4
    | D5 = 5
    | D6 = 6
    | D8 = 8
    | D10 = 10
    | D12 = 12
    | D20 = 20
    | D100 = 100
type Roll =
    | Zero
    | Const of int
    | D of (int * DiceNum)
    | Aggregate of Roll list * (int list -> int)
    | Transform of Roll list * (int list -> int list)
    | Sublist of Roll list
with
    static member aggregate f : Roll list -> Roll = fun l -> Aggregate (l, f)
    static member chooseMin = Roll.aggregate (List.reduce min)
    static member chooseMax = Roll.aggregate (List.reduce max)
    static member sum = Roll.aggregate List.sum
    static member sub = Roll.aggregate (List.reduce (-))
    static member avg = Roll.aggregate (List.averageBy float >> int)
    static member dropAll (p : int -> bool) (l : Roll list) : Roll = Transform (l, List.filter (p >> not))
    static member dropWorst (n : int) (l : Roll list) : Roll = Transform (l, List.sort >> List.skip n)
    static member dropBest (n : int) (l : Roll list) : Roll = Transform (l, List.sortDescending >> List.skip n)
    
    static member chooseMin' = List.wrap >> Roll.chooseMin
    static member chooseMax' = List.wrap >> Roll.chooseMax
    static member sum' = List.wrap >> Roll.sum
    static member sub' = List.wrap >> Roll.sub
    static member avg' = List.wrap >> Roll.avg
    static member dropAll' p = List.wrap >> Roll.dropAll p
    static member dropWorst' n = List.wrap >> Roll.dropWorst n
    static member dropBest' n = List.wrap >> Roll.dropBest n

    static member ( * ) (d : DiceNum, n : int) = D (n, d)
    static member ( * ) (n : int, d : DiceNum) = D (n, d)
    
    static member ( + ) (r1 : Roll, r2 : Roll) = Roll.sum [r1; r2]
    static member ( + ) (r1 : Roll, r2 : (int * DiceNum)) = Roll.sum [r1; D r2]
    static member ( + ) (r1 : Roll, r2 : DiceNum) = r1 + (1, r2)
    
    static member ( + ) (r1 : (int * DiceNum), r2 : Roll) = Roll.sum [D r1; r2]
    static member ( + ) (r1 : DiceNum, r2 : Roll) = Roll.sum [D (1, r1); r2]
    
    static member ( - ) (r1 : Roll, r2 : Roll) = Roll.sub [r1; r2]
    
let d n = (1, enum<DiceNum> n) |> D

type Resolution<'t> =
    | Resolved of 't
    | Unresolved of 't
    
module Res =
    let toOption f = function
        | Resolved x -> Some (f x)
        | Unresolved x -> None
    let bind f = function
        | Resolved x -> Resolved x
        | Unresolved x -> f x
    let map f = function
        | Resolved x -> Resolved x
        | Unresolved x -> Unresolved (f x)
    let mapResolved f = function
        | Resolved x -> Resolved (f x)
        | Unresolved x -> Unresolved x
    let resolve = function
        | Resolved _ as x -> x
        | Unresolved x -> Resolved x
    let mapIf cond f = function
        | Unresolved x -> (if cond x then f x else x) |> Unresolved
        | x -> x
    let resolveIf cond = function
        | Unresolved x -> if cond x then Resolved x else Unresolved x
        | x -> x
    let mapResolveIf cond f x =
        x |> map f |> resolveIf cond
    let whenResolved f = function
        | Resolved _ as x -> f x
        | x -> x
    let whenUnresolved f = function
        | Unresolved _ as x -> f x
        | x -> x
    let failIfUnresolved = function
        | Unresolved x -> failwith "Resolution Exception"
        | x -> x
    let mapAny f = function
        | Resolved x -> Resolved (f x)
        | Unresolved x -> Unresolved (f x)
    let bindAny f = function
        | Resolved x -> f x
        | Unresolved x -> f x
    let ifElse fres funres = function
        | Resolved x -> fres x
        | Unresolved x -> funres x
        
module Deck =
    open Helpers

    let frenchSuits = [Diamonds; Hearts; Spades; Clubs]
    let frenchRanks = (List.init 10 (inc >> Number) @ [Jack; Queen; King])

    let tarotSuits = [Swords; Cups; Coins; Wands]
    let tarotRanks = (List.init 10 (inc >> Number) @ [Jack; Knight; Queen; King])


    let french = 
        [|
            yield MinorCard (Red, Joker)
            yield MinorCard (Black, Joker) 
            for suit in frenchSuits do
                for rank in frenchRanks do
                    yield MinorCard (suit, rank)
        |] 

    let tarot =
        let minorArcana =
            [
                for suit in tarotSuits do
                    for rank in tarotRanks do
                        yield MinorCard (suit, rank)
            ] 
        let majorArcana =
            List.init 22 (enum<ImageCard> >> SpecialCard)
        (minorArcana @ majorArcana) |> List.toArray



module Roll =
    open Helpers
    let private rand = System.Random ()
    
    let rec getResult randomizer = function
        | Zero -> [0]
        | Const n -> [n]
        | D (count, dice) -> randomizer count (int dice)
        | Aggregate (l, f) -> [l |> List.collect (getResult randomizer) |> f]
        | Transform (l, f) -> l |> List.collect (getResult randomizer) |> f
        | Sublist l -> l |> List.collect (getResult randomizer)
    
    let throwMany = 
        (fun count dice -> List.init count (fun _ -> nextInt 1 dice rand))
        |> getResult

    let bestMany =
        (fun count dice -> [count * dice])
        |> getResult
        
    let worstMany =
        (fun count dice -> [1])
        |> getResult

    let throw = throwMany >> List.sum
    let best = bestMany >> List.sum
    let worst = worstMany >> List.sum
        
    let average iterations roll = 
        [1..iterations] |> List.averageBy (fun _ -> throw roll |> float)
        
    let successProb iterations pred roll =
        [1..iterations] |> List.averageBy (fun _ -> throw roll |> pred |> Helpers.boolToFloat)
        
    let resolve difficulty roll =
        let v = throw roll  
        if v > difficulty then Resolved (v - difficulty) else Unresolved (difficulty - v) 
        
type Point = {
    Clean : int
    Spent : int
    Damaged : int
    Injured : int
    Total : int 
}

type StylePoint = {
    Root : int
    Current : int
    Maximum : int
}

type Style =
    | Direct of string * string
    | Opposite of string * string

type SkillPoint = {
    Level : int
    Spent : int
    Total : int
}

module Point =
    open Helpers

    let create n = {
        Clean = n
        Spent = 0
        Damaged = 0
        Injured = 0
        Total = 10
    }

    let mapClean f p : Point = { p with Clean = f p.Clean }
    let mapSpent f p : Point = { p with Spent = f p.Spent }
    let mapDamaged f p : Point = { p with Damaged = f p.Damaged }
    let mapInjured f p : Point = { p with Injured = f p.Injured }
    let mapTotal f p : Point = { p with Total = f p.Total }
    
    let hasClean (p : Point) = p.Clean > 0
    let hasSpent (p : Point) = p.Spent > 0
    let hasDamaged (p : Point) = p.Damaged > 0
    let hasInjured (p : Point) = p.Injured > 0

    let injury =
        Unresolved
        >> Res.mapResolveIf hasClean (mapClean dec)
        >> Res.mapResolveIf hasSpent (mapSpent dec)
        >> Res.mapResolveIf hasDamaged (mapDamaged dec)
        >> Res.mapResolved (mapInjured inc)
    
    let damage =
        Unresolved
        >> Res.mapResolveIf hasClean (mapClean dec)
        >> Res.mapResolveIf hasSpent (mapSpent dec)
        >> Res.mapResolved (mapDamaged inc)
        >> Res.bind injury
        
    let spent =
        Unresolved
        >> Res.mapResolveIf hasClean (mapClean dec)
        >> Res.mapResolved (mapSpent inc)
        
    let clearDamaged =
        Unresolved
        >> Res.mapResolveIf hasDamaged (mapDamaged dec)
        >> Res.mapResolved (mapClean inc)
        
    let clearSpent =
        Unresolved
        >> Res.mapResolveIf hasSpent (mapSpent dec)
        >> Res.mapResolved (mapClean inc)
        
    let clearInjured =
        Unresolved
        >> Res.mapResolveIf hasInjured (mapInjured dec)
        >> Res.mapResolved (mapClean inc)
        
module StylePoint =
    open Helpers
    
    let create n = {
        Root = n
        Current = n
        Maximum = 10
    }

    let mapCurrent f p : StylePoint = { p with Current = f p.Current }
    let mapRoot f p : StylePoint = { p with Current = f p.Root }
    
    let shift : StylePoint -> StylePoint Resolution = 
        Unresolved
        >> Res.mapIf (fun x -> x.Current < x.Maximum) (mapCurrent inc)
        
    let shiftOpposite : StylePoint -> StylePoint Resolution = 
        Unresolved
        >> Res.mapIf (fun x -> x.Current > 0) (mapCurrent dec)
    
    let adjust (p : StylePoint) =
        if p.Current < p.Root then mapRoot dec p
        elif p.Current > p.Root then mapRoot inc p
        else p
        
module SkillPoint =
    open Helpers
    
    let create level n = {
        Level = level
        Spent = 0
        Total = n
    }
    
    let mapLevel f p : SkillPoint = { p with Level = f p.Level }
    let mapSpent f p : SkillPoint = { p with Spent = f p.Spent }
    
    let getAvailable (p : SkillPoint) = p.Total - p.Spent
    
    let spend : SkillPoint -> SkillPoint Resolution =
        Unresolved
        >> Res.resolveIf (fun x -> getAvailable x > 0)
        >> Res.mapResolved (mapSpent inc)
        
    let restore : SkillPoint -> SkillPoint Resolution =
        Unresolved
        >> Res.resolveIf (fun x -> getAvailable x < x.Total)
        >> Res.mapResolved (mapSpent dec)
        
  type Time = {
    ActionStep : int
    SceneStep : int
    Day : int
    Year : int
}
type Character = {
    Attributes : Map<string, Point>
    Style : Map<(string * string), StylePoint>
    Skills : Map<string, SkillPoint>
}

module Attribute =
    open Point 
    
    let roll p =
        if hasClean p || hasSpent p then
            match p.Clean with
            | x when x < 3 -> d 4
            | x when x < 5 -> d 6
            | x when x < 7 -> d 8
            | x when x < 9 -> d 10
            | x when x < 11 -> d 12
            | _ -> d 20
        else Zero
    
    let restRoll (p : Point) =
        match p.Total - p.Injured with
        | x when x < 3 -> d 4
        | x when x < 5 -> d 6
        | x when x < 7 -> d 8
        | x when x < 9 -> d 10
        | x when x < 11 -> d 12
        | _ -> d 20

module StyleRolls =
    open StylePoint 
    
    let getRoll p =
        match p.Current with
        | x when x < 3 -> d 4
        | x when x < 5 -> d 6
        | x when x < 7 -> d 8
        | x when x < 9 -> d 10
        | x when x < 11 -> d 12
        | _ -> d 20
        
    let getOppositeRoll p =
        match p.Current with
        | x when x <= 2 -> d 12
        | x when x <= 4 -> d 10
        | x when x <= 6 -> d 8
        | x when x <= 8 -> d 6
        | x when x <= 10 -> d 4
        | _ -> d 4
        
module SkillRolls =
    open SkillPoint
    
    let roll p =
        match getAvailable p with
        | 1 -> d 4
        | 2 -> d 6
        | 3 -> d 8
        | 4 -> d 10
        | 5 -> d 12
        | _ -> Zero
        
    let restRoll p = 
        match p.Total with
        | 1 -> d 6
        | 2 -> d 8
        | 3 -> d 10
        | 4 -> d 12
        | 5 -> d 20
        | _ -> Zero
        
module Attributes =
    let strength = "strength"
    let dexterity = "dexterity"
    let appearance = "appearance"
    let intelligence = "intelligence"
    let willpower = "willpower"
    let perception = "perception"
    
    let attributeList = [
        strength
        dexterity
        appearance
        intelligence
        willpower
        perception
    ]
    
    module Contractions =
        let str = strength
        let dex = dexterity
        let app = appearance
        let intl = intelligence
        let wp = willpower
        let perc = perception
        
module Styles =
    open Helpers
    let private style = applyTwice Direct Opposite

    let expression, selfControl = style ("expression", "selfControl")
    let faith, rationality = style ("faith", "rationality")
    let empathy, coldBlood = style ("empathy", "coldBlood")
    let trust, misTruct = style ("trust", "mistrust")
    let swiftness, accuracy = style ("swiftness", "accuracy")
    let experiment, knowledge = style ("experiment", "knowledge")
    
    let styleList = [
        expression
        faith
        empathy
        trust
        swiftness
        experiment
    ]
    
type RollComponent =
    | AttributeComponent of string
    | SkillComponent of string
    | StyleComponent of Style
    
module Character =
    module Attribute =
        let get name (ch : Character) = ch.Attributes.[name]
        let set name v (ch : Character) = { ch with Attributes = ch.Attributes |> Map.add name v }
        let map f name (ch : Character) = { ch with Attributes = ch.Attributes |> Map.add name (f <| get name ch) }

        let roll attr = get attr >> Attribute.roll
        let restRoll attr = get attr >> Attribute.restRoll
        
        let generate () =
            let roll = Roll.dropWorst 2 >> (fun x -> [x]) >> Roll.dropBest 2 >> Roll.throwMany >> List.max
            let dices = List.replicate 6 (d 10)
            
            Point.create (roll dices) 
        
    module Style =
        let get name (ch : Character) =
            match name with | Opposite (a,b) | Direct (a,b) -> ch.Style.[(a,b)]
        let set name v (ch : Character) =
            match name with | Opposite (a,b) | Direct (a,b) -> { ch with Style = ch.Style |> Map.add (a,b) v }
        let map f name (ch : Character) = 
            match name with | Opposite (a,b) | Direct (a,b) -> { ch with Style = ch.Style |> Map.add (a,b) (f <| get name ch) }
            
        let roll style ch =
            match style with
            | Direct _ -> get style ch |> StyleRolls.getRoll
            | Opposite _ -> get style ch |> StyleRolls.getOppositeRoll
            
        let generate () =
            StylePoint.create (Roll.throw (d 10))
        
    module Skills =
        let get name (ch : Character) = 
            if Map.containsKey name ch.Skills then ch.Skills.[name] else { Level = 1; Spent = 0; Total = 0 }
        let set name v (ch : Character) = { ch with Skills = ch.Skills |> Map.add name v }
        let map f name (ch : Character) = { ch with Attributes = ch.Attributes |> Map.add name (f <| get name ch) }
        
        let roll skill (ch : Character) = (get skill >> SkillRolls.roll) ch
        let restRoll skill (ch : Character) = (get skill >> SkillRolls.restRoll) ch
    
    let empty = {
        Attributes = Map.empty
        Skills = Map.empty
        Style = Map.empty
    }
    
    open Attributes
    open Skills
    open Styles
    
    let createRandom () =
        let fillAttrs = 
            attributeList
            |> List.map (fun x -> Attribute.set x (Attribute.generate ()))
            |> List.reduce (>>)
        let fillStyles =
            styleList
            |> List.map (fun x -> Style.set x (Style.generate ()))
            |> List.reduce (>>)
        empty
        |> fillAttrs
        |> fillStyles
        


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// Штраф 
//      |  1  |  2   |  3    |  4
// d100 | 1%  | 2%   | 3%    | 4%
// d20  | 5%  | 10%  | 15%   | 20%
// d12  | 8%  | 16%  | 25%   | 33%
// d10  | 10% | 20%  | 30%   | 40%
// d8   | 12% | 25%  | 37%   | 50%
// d6   | 16% | 33%  | 50%   | 66%
// d4   | 25% | 50%  | 75%   | 100%
// d2   | 50% | 100% | 100%  | 100%
open Helpers

let diff = 2
let roll = d 10

List.init 4 (fun i -> List.replicate (i + 1) roll)
|> List.map (Roll.chooseMin)
|> List.map (Roll.successProb 100000 (binv (>) diff))
|> List.pairwise
|> List.map (fun (a, b) -> (a - b) / a * 100.0)

open MathNet.Numerics.Statistics
open FSharp.Plotly

let _3d8_choose_min_distribution =
    List.init 10000 (fun _ -> [d 8; d 8; d 8])
    |> List.map (Roll.chooseMin >> Roll.throw)
    |> Chart.Histogram

let _3d6_choose_min_distribution =
    List.init 10000 (fun _ -> [d 6; d 6; d 6])
    |> List.map (Roll.chooseMin >> Roll.throw)
    |> Chart.Histogram

let _3d8_sniper_ability_check_distribution_dif2 =
    List.init 10000 (fun _ -> 
        if Roll.throw (d 8) > 2 then
            [d 8; d 8] |> Roll.chooseMin |> Roll.throw
        else
            [d 8; d 8; d 8] |> Roll.chooseMin |> Roll.throw
    )    
    |> Chart.Histogram

let _2d8_choose_min_distribution =
    List.init 10000 (fun _ -> [d 8; d 8;])
    |> List.map (Roll.chooseMin >> Roll.throw)
    |> Chart.Histogram


_3d8_choose_min_distribution
|> Chart.SaveHtmlAs "./_3d8_choose_min_distribution"

_3d8_sniper_ability_check_distribution_dif2
|> Chart.SaveHtmlAs "./_3d8_sniper_ability_check_distribution_dif2"


[
    _3d8_choose_min_distribution
    _3d8_sniper_ability_check_distribution_dif2
    _2d8_choose_min_distribution
    _3d6_choose_min_distribution
]
|> Chart.Combine
|> Chart.SaveHtmlAs "./test"

let _mdn_choose_min_distribution m n =
    List.init 10000 (fun _ -> List.replicate m (d n))
    |> List.map (Roll.chooseMin >> Roll.throw)
    |> Chart.Histogram

[
    _mdn_choose_min_distribution 2 4
    _mdn_choose_min_distribution 2 6
    _mdn_choose_min_distribution 2 8
    _mdn_choose_min_distribution 2 10
    _mdn_choose_min_distribution 2 12
    _mdn_choose_min_distribution 2 20
]
|> Chart.Combine
|> Chart.SaveHtmlAs "./test"
