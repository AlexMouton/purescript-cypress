module Cypress.Chai where

import Prelude

import Data.String.Regex (Regex)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4)

import Cypress.Cy (CypressM)
import Cypress.Ask (naskC2, naskC3, naskC4)
import Cypress.Elements (Elements)
-- import Cypress.Actions
import Cypress.Query (Query)

-- Deep	-- expect(obj).to.deep.equal({ name: 'Jane' })
-- Nested	-- expect({a: {b: ['x', 'y']}}).to.have.nested.property('a.b[1]')
-- -- expect({a: {b: ['x', 'y']}}).to.nested.include({'a.b[1]': 'y'})
-- Ordered	-- expect([1, 2]).to.have.ordered.members([1, 2]).but.not.have.ordered.members([2, 1])
-- Any	-- expect(arr).to.have.any.keys('age')
-- All	-- expect(arr).to.have.all.keys('name', 'age')
-- A(type)
-- An(type) -- expect('test').to.be.a('string')
-- Include(value)
-- -- Aliases: contain, includes, contains	-- expect([1,2,3]).to.include(2)
-- Ok	-- expect(undefined).to.not.be.ok
-- Null	-- expect(null).to.be.null
-- Undefined	-- expect(undefined).to.be.undefined
-- Exist	-- expect(myVar).to.exist
-- Empty	-- expect([]).to.be.empty
-- Arguments
-- -- Aliases: Arguments	-- expect(arguments).to.be.arguments
-- DeepEqual(value)	-- expect({ name: 'Jane' }).to.deep.equal({ name: 'Jane' })

-- InstanceOf(constructor)
-- -- Aliases: instanceof	-- expect([1, 2, 3]).to.be.instanceOf(Array)
-- Property(name, [value])	-- expect(obj).to.have.property('name')
-- DeepProperty(name, [value])	-- expect(deepObj).to.have.deep.property('tests[1]', 'e2e')
-- OwnProperty(name)
-- HaveOwnProperty-- Aliases: haveOwnProperty, own.property	-- expect('test').to.have.ownProperty('length')
-- OwnPropertyDescriptor(name)
-- -- Aliases: haveOwnPropertyDescriptor	-- expect({a: 1}).to.have.ownPropertyDescriptor('a')
-- Keys(key1, [key2], […])
-- -- Aliases: key	-- expect({ pass: 1, fail: 2 }).to.have.keys('pass', 'fail')
-- Throw(constructor)
-- -- Aliases: throws, Throw	-- expect(fn).to.throw(Error)
-- RespondTo(method)
-- -- Aliases: respondsTo	-- expect(obj).to.respondTo('getName')
-- Itself	-- expect(Foo).itself.to.respondTo('bar')
-- Satisfy(method)
-- -- Aliases: satisfies	-- expect(1).to.satisfy((num) => { return num > 0 })
-- Members(set)	-- expect([1, 2, 3]).to.include.members([3, 2])
-- Change(function)
-- -- Aliases: changes	-- expect(fn).to.change(obj, 'val')
-- Increase(function)
-- -- Aliases: increases	-- expect(fn).to.increase(obj, 'val')
-- Decrease(function)
-- -- Aliases: decreases	-- expect(fn).to.decrease(obj, 'val')


class ShouldStr a where
  toStr ::  a -> String

class ShouldStr a <= Should a b where
  toShould :: a -> (Query b) -> CypressM (Query b)
  toShould' :: String -> a -> (Query b) -> CypressM (Query b)

foreign import should0Fn :: forall a. EffectFn2 String (Query a) (Query a)
foreign import should1Fn :: forall a b. EffectFn3 String b (Query a) (Query a)
foreign import should2Fn :: forall a b. EffectFn4 String b b (Query a) (Query a)

data True = True-- expect(true).to.be.true
instance shouldStrTrue :: ShouldStr True where
  toStr _ = "be.true"

instance shouldTrue :: Should True Boolean where
  toShould a = toShould' (toStr a) a
  toShould' s True = naskC2 should0Fn s

data False = False -- expect(false).to.be.false
instance shouldStrFalse :: ShouldStr False where
  toStr _ = "be.false"

instance shouldFalse :: Should False Boolean where
  toShould a = toShould' (toStr a) a
  toShould' s False = naskC2 should0Fn s


-- Match(RegExp)
-- -- Aliases: matches	-- expect('testing').to.match(/^test/)
newtype Match = Match Regex
instance shouldStrMatch :: ShouldStr Match where
  toStr _ = "to.match"

instance shouldMatch :: Should Match String where
  toShould a = toShould' (toStr a) a
  toShould' s (Match re) = naskC3 should1Fn s re


-- String(string)	-- expect('testing').to.have.string('test')
newtype SubString = SubString String
instance shouldStrSubString :: ShouldStr SubString where
  toStr _ = "to.have.string"

instance shouldSubString :: Should SubString String where
  toShould a = toShould' (toStr a) a
  toShould' s (SubString sub) = naskC3 should1Fn s sub


-- CloseTo(expected, delta)
-- -- Aliases: approximately	-- expect(1.5).to.be.closeTo(1, 0.5)
data CloseTo = CloseTo Number Number
instance shouldStrCloseTo :: ShouldStr CloseTo where
  toStr _ = "be.closeTo"

instance shouldCloseTo :: Should CloseTo Number where
  toShould a = toShould' (toStr a) a
  toShould' s (CloseTo a b) = naskC4 should2Fn s a b

-- Equal(value)
-- -- Aliases: equals, eq	-- expect(42).to.equal(42)
-- Eql(value)
-- -- Aliases: eqls	-- expect({ name: 'Jane' }).to.eql({ name: 'Jane' })
newtype Equal a = Equal a
instance shouldStrEq :: ShouldStr (Equal a) where
  toStr _ = "equal"

instance shouldEqual :: Eq a => Should (Equal a) a where
  toShould a = toShould' (toStr a) a
  toShould' s (Equal a) = naskC3 should1Fn s a

-- GreaterThan(value)
-- -- Aliases: gt, above	-- expect(10).to.be.greaterThan(5)
newtype GreaterThan a = GreaterThan a
instance shouldStrGreaterThan :: ShouldStr (GreaterThan a) where
  toStr _ = "be.greaterThan"

instance shouldGreaterThan :: Ord a => Should (GreaterThan a) a where
  toShould a = toShould' (toStr a) a
  toShould' s (GreaterThan a) = naskC3 should1Fn s a

-- LessThan(value)
-- Lt(value)
-- Below(value) -- Aliases: lt, below	-- expect(5).to.be.lessThan(10)
newtype LessThan a = LessThan a
instance shouldStrLessThan :: ShouldStr (LessThan a) where
  toStr _ = "be.lessThan"

instance shouldLessThan :: Ord a => Should (LessThan a) a where
  toShould a = toShould' (toStr a) a
  toShould' s (LessThan a) = naskC3 should1Fn s a

-- Least(value)
-- Gte(value)-- Aliases: gte	-- expect(10).to.be.at.least(10)
newtype AtLeast a = AtLeast a
instance shouldStrAtLeast :: ShouldStr (AtLeast a) where
  toStr _ = "at.least"

instance shouldAtLeast :: Ord a => Should (AtLeast a) a where
  toShould a = toShould' (toStr a) a
  toShould' s (AtLeast a) = naskC3 should1Fn s a

-- Most(value)
-- Lte(value) -- Aliases: lte	-- expect('test').to.have.length.of.at.most(4)
newtype AtMost a = AtMost a
instance shouldStrAtMost :: ShouldStr (AtMost a) where
  toStr _ = "at.most"

instance shouldAtMost :: Ord a => Should (AtMost a) a where
  toShould a = toShould' (toStr a) a
  toShould' s (AtMost a) = naskC3 should1Fn s a

-- Within(start, finish)	-- expect(7).to.be.within(5,10)
data Within a = Within a a
instance shouldStrWithin :: ShouldStr (Within a) where
  toStr _ = "be.within"

instance shouldWithin :: Ord a => Should (Within a) a where
  toShould a = toShould' (toStr a) a
  toShould' s (Within a b) = naskC4 should2Fn s a b


-- OneOf(values)	-- expect(2).to.be.oneOf([1,2,3])
newtype OneOf a = OneOf (Array a)
instance shouldStrOneOf :: ShouldStr (OneOf a) where
  toStr _ = "be.oneOf"

instance shouldOneOf :: Eq a => Should (OneOf a) a where
  toShould a = toShould' (toStr a) a
  toShould' s (OneOf is) = naskC3 should1Fn s is


newtype LengthOf a = LengthOf Int

instance shouldStrLength :: ShouldStr (LengthOf a) where
  toStr _ = "have.length"

instance shouldLengthArray :: Should (LengthOf (Array a)) (Array a) where
  toShould a = toShould' (toStr a) a
  toShould' s (LengthOf i) = naskC3 should1Fn s i

instance shouldLengthElements :: Should (LengthOf Elements) Elements where
  toShould a = toShould' (toStr a) a
  toShould' s (LengthOf i) = naskC3 should1Fn s i


newtype Visible = Visible Unit
instance shouldStrVisible :: ShouldStr Visible where
  toStr _ = "be.visible"

instance shouldVisible :: Should Visible Elements where
  toShould a = toShould' (toStr a) a
  toShould' s _ = naskC2 should0Fn s


-- Not -- expect(name).to.not.equal('Jane')
newtype Not a = Not a

instance shouldStrNot :: ShouldStr a => ShouldStr (Not a) where
  toStr (Not a) = "not." <> toStr a

instance shouldNot :: Should a b => Should (Not a) b where
  toShould a = toShould' (toStr a) a
  toShould' s (Not a) = toShould' s a