# my-cc

Cコンパイラ
実装は https://www.sigbus.info/compilerbook を参考にしている

またテストとして hsjoihs さんのテストケースをmy-cc向けに修正して利用している
https://github.com/hsjoihs/c-compiler/blob/master/test_cases.sh

## selfhost
`make mycc2`

## テスト
第一世代のテストは `make external-test`

第二世代のテストは `make external-test-stage2`

