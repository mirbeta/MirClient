{
  "Generic Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit rtcSrcTree;

{$INCLUDE rtcDefs.inc}

{$DEFINE RTC_GENERIC}
{$DEFINE RTC_BINTREE}

interface

uses
  SysUtils,
  Generics.Defaults,
  rtcTypes, memPtrPool;

type
  tRtcSearchTree<itemType,infoType>={$I sort\class.inc};

implementation

function tRtcSearchTree<itemType,infoType>.{$I sort\Empty.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\NewNode.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\PoolSize.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\DelNode.inc};

constructor tRtcSearchTree<itemType,infoType>.{$I sort\Create.inc};

constructor tRtcSearchTree<itemType,infoType>.{$I sort\Create2.inc};

constructor tRtcSearchTree<itemType,infoType>.{$I sort\Create3.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\Change.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\RemoveThis.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\RemoveAll.inc};

destructor tRtcSearchTree<itemType,infoType>.{$I sort\Destroy.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\Search.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\iSearch.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\SearchMin.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\iSearchMin.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\SearchMax.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\iSearchMax.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\SearchL.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\iSearchL.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\SearchG.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\iSearchG.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\SearchLE.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\iSearchLE.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\SearchGE.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\iSearchGE.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\InsertSplit.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\InsertSplit2.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\Insert.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\RemoveAddP.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\RemoveGetP.inc};

procedure tRtcSearchTree<itemType,infoType>.{$I sort\Remove.inc};

function tRtcSearchTree<itemType,infoType>.{$I sort\Count.inc};

end.

