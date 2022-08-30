{
  "Generic Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit rtcSrcList;

{$INCLUDE rtcDefs.inc}

{$DEFINE RTC_GENERIC}

interface

uses
  SysUtils,
  Generics.Defaults,

  rtcTypes, memPtrPool;

type
  tRtcSearchList<itemType,infoType>={$I sort\class.inc};

implementation

function tRtcSearchList<itemType,infoType>.{$I sort\Empty.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\NewNode.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\PoolSize.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\DelNode.inc};

constructor tRtcSearchList<itemType,infoType>.{$I sort\Create.inc};

constructor tRtcSearchList<itemType,infoType>.{$I sort\Create2.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\Change.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\RemoveThis.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\RemoveAll.inc};

destructor tRtcSearchList<itemType,infoType>.{$I sort\Destroy.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\Search.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\SearchMin.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\SearchMax.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\SearchL.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\SearchG.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\SearchLE.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\SearchGE.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\InsertSplit.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\Insert.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\RemoveAddP.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\RemoveGetP.inc};

procedure tRtcSearchList<itemType,infoType>.{$I sort\Remove.inc};

function tRtcSearchList<itemType,infoType>.{$I sort\Count.inc};

end.

