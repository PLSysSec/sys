module OldRegression where

-- These regression tests are not as applicable anymore---they were from early development
-- of the tool, and some of the checkers they rely on we decided weren't that interesting
-- (e.g., null pointer because no one cares about null pointer bugs). Still, this file
-- remains because it catalogs some early gotchas in our development process.

-- regressionTests :: BenchTest
-- regressionTests = benchTestGroup "End-to-end regression tests" [ pathological
--                                                                , unknownConstant
--                                                                , selfRef
--                                                                , opaque
--                                                                , unsupportedTy
--                                                                , unsupportedTyInStruct
--                                                                , manyPointers
--                                                                , smallJumps
--                                                                , nodeuaffp1
--                                                                , nodeuaffp2
--                                                                , nodeuaffp3
--                                                                , nodeuaffp4
--                                                                , offsetCheck
--                                                                , nodeuaffp5
--                                                                , i8array
--                                                                , nodeuaffp6
--                                                                , nodeuaffp7
--                                                                , ffuaffp1
--                                                                , ffuaffp2
--                                                                , ffuaffn1
--                                                                , ffuaffp3
--                                                                , ffuaffp4
--                                                                ]

-- regPath :: String
-- regPath = "test/Regression/"

-- -- This bug caused simple programs to timeout.
-- -- It happened because we were allocating every pointer in a structure
-- -- when that structure was used, instead of allocating pointers themselves
-- -- as we loaded off of them and stored to them.
-- pathological :: BenchTest
-- pathological = let quietUaf = (uafConfig 5 5) { verbose = False }
--                in basicAllUnsatTest quietUaf $ regPath ++ "pathologicalUnbounded.ll"

-- -- We used to "error" on unknown constants, but now we leave them unconstrained
-- unknownConstant :: BenchTest
-- unknownConstant = let expectedVars = M.fromList [ ("stderr", 64)
--                                                 , ("add_ptr", 128)
--                                                 ]
--                   in variableAssignmentTest expectedVars $ regPath ++ "unknownConst.ll"

-- -- We used to "error" on self-referential types (for no reason), but
-- -- now we handle them properly (just got rid of the arbitrary "error" call)
-- selfRef :: BenchTest
-- selfRef = basicAllSatTest testConfig $ regPath ++ "selfRefTy.ll"

-- -- We used to give up on opaque types. Now, we don't, since an opaque type
-- -- implies that only a pointer to the type will be used---and we know the size
-- -- of a pointer.
-- opaque :: BenchTest
-- opaque = basicAllSatTest testConfig $ regPath ++ "opaqueTy.ll"

-- -- We used to error very later when hitting an unsupported type (ie during symbolic
-- -- execution). Now, when we hit an unsupported type (eg float), we error before
-- -- beginning symbolic execution at all. And now, in the static phase
-- unsupportedTy :: BenchTest
-- unsupportedTy = basicDontAnalyzeTest testConfig { verbose = True }
--                 $ regPath ++ "unsupportedTy.ll"

-- -- We used to error if any type in a struct, even an unused one, was
-- -- an unsupported type. We should instead be able to execute those paths
-- -- This test also uncovered another bug, this time in argument alias:
-- -- we set all struct arguments not to alias, including to themselves.
-- unsupportedTyInStruct :: BenchTest
-- unsupportedTyInStruct = basicAllSatTest testConfig
--                         $ regPath ++ "unsupportedTyInStruct.ll"

-- -- This is to test that our lazy loading strategy doesn't take an insane amount
-- -- of time when it has to query the solver at every load of an unknown pointer
-- manyPointers :: BenchTest
-- manyPointers = basicAllSatTest testConfig $ regPath ++ "manyPtrs.ll"

-- -- This is to test that our array jump bound is correct.
-- -- We used to only constraint single GEP jumps (eg x = gep y z, z must be <= 10)
-- -- This allowed chained GEPs to slam into other memory:
-- -- array bound: 10
-- -- x = gep y z, z <= 10
-- -- w = gep x k, k <= 10
-- -- but now we've jumped 20 off of x, which puts us squarely in some other obj's memory
-- smallJumps :: BenchTest
-- smallJumps = basicAllUnsatTest (uafTest 1) $ regPath ++ "smallJumps.ll"

-- -- This bug happened because any time we GEP'd with an array index of
-- -- zero, we set the heap allocation bound to 0, so everything was
-- -- heap allocated in exactly the same spot
-- nodeuaffp1 :: BenchTest
-- nodeuaffp1 = basicAllUnsatTest (uafTest 3) $ regPath ++ "nodeuaffp1.ll"

-- -- This should be SAT but is unsat because of how we do loop versioning
-- -- It doesn't version an in-loop variable, so it has sets a variable
-- -- simultaneously equal to two different values.
-- nodeuaffp2 :: BenchTest
-- nodeuaffp2 = basicAllSatTest (testConfig { pathLength = 3 }) $ regPath ++ "nodeuaffp2.ll"

-- -- This bug happened because we attacked lines of code that were never "used"
-- -- and therefore never symbolically executed:
-- -- 1. pointers get lazily assigned concrete locations on loads and stores
-- -- 2. because symex wasn't symexing the line, the loaded pointer never got
-- --    assigned a location
-- -- 3. the checker still queried to see if the unused pointer was equal to the freed ptr
-- -- 4. since there was no concrete location assigned for the unused ptr,
-- --    the unused ptr == freed ptr, and the checker reported a bug
-- nodeuaffp3 :: BenchTest
-- nodeuaffp3 = basicAllUnsatTest (uafTest 2) $ regPath ++ "nodeuaffp3.ll"

-- -- This bug was caused by incorrect offset calculation.
-- -- This line:
-- -- %new86 = getelementptr inbounds %"class.node::Environment", %"class.node::Environment"*
-- --   %fake48, i64 0, i32 14, i32 0, i32 3
-- -- yielded a pointer with a value not aligned to 64.
-- -- This line:
-- -- %new87 = load i32*, i32** %new86
-- -- read from the unaligned pointer, yielding a shorter width than 64.
-- -- this meant that 87 *could not* be equal to the concrete allocation
-- -- that the solver tried to assign to it.
-- -- Then, in the checking passed, since %new87 was unconstrained, it
-- -- took on the value of the freed pointer.
-- nodeuaffp4 :: BenchTest
-- nodeuaffp4 = basicAllUnsatTest (uafTest 1) $ regPath ++ "nodeuaffp4.ll"

-- -- This is a followup to the previous test to check offseting
-- offsetCheck :: BenchTest
-- offsetCheck = basicAllSatTest testConfig $ regPath ++ "offset.ll"

-- -- This bug is again caused by loop bounding.
-- -- When we determine the static loop bound, we don't consider loops.
-- -- This means that we'll consider the following code as having a
-- -- maximum heap allocation jump of one:
-- -- while (y < 3)
-- --  pointer x = x + 64
-- --  y--
-- -- This is incorrect: we should end up with a bound of at least 3,
-- -- so that jumps off x won't smash into other peices of memory
-- nodeuaffp5 :: BenchTest
-- nodeuaffp5 = basicAllUnsatTest (uafTest 4) $ regPath ++ "nodeuaffp5.ll"

-- -- Sizing sanity check.
-- i8array :: BenchTest
-- i8array = basicAllSatTest testConfig $ regPath ++ "i8array.ll"

-- -- This bug happened because we literally were not executing path
-- -- constraints or tracking trackops
-- nodeuaffp6 :: BenchTest
-- nodeuaffp6 = basicAnySatTest (uafTest 2) $ regPath ++ "nodeuaffp6.ll"

-- -- This bug is a result of branch conditions interacting with
-- -- lazy pointer allocation:
-- -- p = load q (concrete allocate q as 2)
-- -- branch condition: w < q
-- -- t = load w (try to concrete allocate w as 3. 3 > 2, though,
-- --            so the result is unsat. the allocation fails)
-- -- now it thinks that w can aliasing anything thats been freed
-- -- As a fix, we quit on anything with direct pointer comparisons
-- nodeuaffp7 :: BenchTest
-- nodeuaffp7 = basicAllUnsatTest (uafTest 4) $ regPath ++ "nodeuaffp7.ll"

-- -- This bug was introduced when we allowed pointers to be null.
-- -- In both strict and non-strict allocation modes, GEP bases
-- -- could be null, allowing GEPs to crash into other memory.
-- -- Now, in strict allocation mode, GEP bases cannot be null.
-- ffuaffp1 :: BenchTest
-- ffuaffp1 = basicAllUnsatTest (uafTest 5) $ regPath ++ "fffp1.ll"

-- -- This bug happened when inner GEP indecies were variables instead of constants.
-- -- In this statement:
-- -- GEP x 0 y
-- -- We are supposed to constrain y to be equal to valid offsets in x.
-- -- Eg, if x is of type { i32, i8, i64 }, it should constrain y to be
-- -- 0, 32, and 64.
-- -- Instead of correctly saying
-- -- x = 0 or x = 32 or x = 64
-- -- We were saying
-- -- x = 0 or 32 or 64
-- ffuaffp2 :: BenchTest
-- ffuaffp2 = basicAllUnsatTest (uafTest 5) $ regPath ++ "fffp2.ll"

-- -- All of these false negatives happened because we were quitting on
-- -- any line that used a metadata type. Firefox builds debug by defauly,
-- -- which includes metadata in lines like this one:
-- --
-- -- llvm.dbg.value(metadata i8* %7, metadata !16576, metadata !DIExpression()) #8, !dbg !16579
-- --
-- -- Interestingly, at no optimization, this is not a UAF, because
-- -- they load the value before the free and store it elsewhere.
-- ffuaffn1 :: BenchTest
-- ffuaffn1 = basicAnySatTest testConfig $ regPath ++ "fffn1.ll"

-- -- This false positive happens when allocations happen to the smaller of
-- -- two "cast" pointers, and geps happen off the larger two:
-- --
-- -- x = bitcast huge* w to i8*
-- -- y = load x (allocate location for x)
-- -- z = gep w 1 1 (gep jumps way beyond allocation, since allocation assumed size 8)
-- --
-- -- To solve this, we are going to keep sets of cast things, and the allocation will
-- -- be to the largest of the items in the set.
-- ffuaffp3 :: BenchTest
-- ffuaffp3 = basicAllUnsatTest ((uafTest 3) { verbose = False }) $ regPath ++ "fffp3.ll"

-- -- This happened because the attack considered all loaded, stored, and argument
-- -- values---including undef. Since undef could be anything, this lead to spurious reports.
-- ffuaffp4 :: BenchTest
-- ffuaffp4 = basicAllUnsatTest ((uafTest 3) { verbose = False }) $ regPath ++ "fffp4.ll"

