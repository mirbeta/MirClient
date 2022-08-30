{
  "Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memStringPtrList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, rtcTypes, memPtrPool;

type
  itemType=RtcWideString;
  infoType=Pointer;

{$I sort\types.inc}

type
  tStringPtrList={$I sort\class.inc};

implementation

const
  itemMin:itemType='';
  infoNil:infoType=nil;

function tStringPtrList.{$I sort\Empty.inc};

function tStringPtrList.{$I sort\NewNode.inc};

procedure tStringPtrList.{$I sort\PoolSize.inc};

procedure tStringPtrList.{$I sort\DelNode.inc};

constructor tStringPtrList.{$I sort\Create.inc};

procedure tStringPtrList.{$I sort\Change.inc};

procedure tStringPtrList.{$I sort\RemoveThis.inc};

procedure tStringPtrList.{$I sort\RemoveAll.inc};

destructor tStringPtrList.{$I sort\Destroy.inc};

function tStringPtrList.{$I sort\Search.inc};

function tStringPtrList.{$I sort\SearchMin.inc};

function tStringPtrList.{$I sort\SearchMax.inc};

function tStringPtrList.{$I sort\SearchL.inc};

function tStringPtrList.{$I sort\SearchG.inc};

function tStringPtrList.{$I sort\SearchLE.inc};

function tStringPtrList.{$I sort\SearchGE.inc};

procedure tStringPtrList.{$I sort\InsertSplit.inc};

procedure tStringPtrList.{$I sort\Insert.inc};

procedure tStringPtrList.{$I sort\RemoveAddP.inc};

function tStringPtrList.{$I sort\RemoveGetP.inc};

procedure tStringPtrList.{$I sort\Remove.inc};

function tStringPtrList.{$I sort\Count.inc};

end.
