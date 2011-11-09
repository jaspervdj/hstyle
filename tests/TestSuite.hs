import Test.Framework (defaultMain)

import qualified HStyle.Block.Tests
import qualified HStyle.Rules.AppSpacing.Tests
import qualified HStyle.Rules.PatMatchAlignment.Tests
import qualified HStyle.Rules.Tabs.Tests
import qualified HStyle.Rules.TrailingWhiteSpace.Tests

main :: IO ()
main = defaultMain
    [ HStyle.Block.Tests.tests
    , HStyle.Rules.AppSpacing.Tests.tests
    , HStyle.Rules.PatMatchAlignment.Tests.tests
    , HStyle.Rules.Tabs.Tests.tests
    , HStyle.Rules.TrailingWhiteSpace.Tests.tests
    ]
