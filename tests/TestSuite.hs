import Test.Framework (defaultMain)

import qualified HStyle.Block.Tests (tests)
import qualified HStyle.Rules.PatMatchAlignment.Tests (tests)
import qualified HStyle.Rules.Tabs.Tests (tests)
import qualified HStyle.Rules.TrailingWhiteSpace.Tests (tests)

main :: IO ()
main = defaultMain
    [ HStyle.Block.Tests.tests
    , HStyle.Rules.PatMatchAlignment.Tests.tests
    , HStyle.Rules.Tabs.Tests.tests
    , HStyle.Rules.TrailingWhiteSpace.Tests.tests
    ]
