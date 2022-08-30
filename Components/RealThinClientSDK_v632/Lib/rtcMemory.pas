{
  @html(<b>)
  Memory Check Unit
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)
  
  @exclude
}

unit rtcMemory;

{$INCLUDE rtcDefs.inc}

{$IFNDEF FPC}
  {$O-}
{$ENDIF}

interface

{ Get Complete Heap Status }
function Get_HeapStatus:THeapStatus;

{ Check the ammount of memoy in use (bytes) }
function Get_MemoryInUse:int64;

{ Check how much Address Space is used by the Application (KB) }
function Get_AddressSpaceUsed:int64;

implementation

{$IFDEF Windows}
uses 
  Windows;
{$ENDIF}

{$ifdef WINDOWS}
function Get_AddressSpaceUsed: int64;
  var
    LMemoryStatus: TMemoryStatus;
  begin
  {Set the structure size}
  LMemoryStatus.dwLength := SizeOf(LMemoryStatus);
  {Get the memory status}
  GlobalMemoryStatus(LMemoryStatus);
  {The result is the total address space less the free address space}
  Result := (LMemoryStatus.dwTotalVirtual - LMemoryStatus.dwAvailVirtual) shr 10;
  end;
{$else}
function Get_AddressSpaceUsed: int64;
  var
    hs :THeapStatus;
  begin
  // no funciton available?
  hs := GetHeapStatus;
  Result := hs.TotalCommitted
  end;
{$endif}

function Get_HeapStatus:THeapStatus;
  begin
  Result:=GetHeapStatus;
  end;

function Get_MemoryInUse:int64;
  begin
  Result:=GetHeapStatus.TotalAllocated;
  end;

end.
