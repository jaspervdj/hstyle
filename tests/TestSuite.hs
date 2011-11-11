import Test.Framework (defaultMain)

import qualified HStyle.Block.Tests
import qualified HStyle.Rules.AppSpacing.Tests
import qualified HStyle.Rules.CaseAlignment.Tests
import qualified HStyle.Rules.DataAlignment.Tests
import qualified HStyle.Rules.LineLength.Tests
import qualified HStyle.Rules.PatMatchAlignment.Tests
import qualified HStyle.Rules.Tabs.Tests
import qualified HStyle.Rules.TrailingWhiteSpace.Tests
import qualified HStyle.Rules.TypeSigAlignment.Tests

main :: IO ()
main = defaultMain
    [ HStyle.Block.Tests.tests
    , HStyle.Rules.AppSpacing.Tests.tests
    , HStyle.Rules.CaseAlignment.Tests.tests
    , HStyle.Rules.DataAlignment.Tests.tests
    , HStyle.Rules.LineLength.Tests.tests
    , HStyle.Rules.PatMatchAlignment.Tests.tests
    , HStyle.Rules.Tabs.Tests.tests
    , HStyle.Rules.TrailingWhiteSpace.Tests.tests
    , HStyle.Rules.TypeSigAlignment.Tests.tests
    ]
