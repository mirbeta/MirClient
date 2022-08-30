{
  @html(<b>)
  Trashcah
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Garbage collection used by the RTC SDK to avoid destroying objects which
  need to remain in memory until all other things have been destroyed.

  @exclude
}
unit rtcTrashcan;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  rtcTypes, rtcLog,
  rtcSyncObjs;

// Add pointer "p" to garbage collector.
procedure Garbage(p:pointer); overload;

// Add object "o" to garbage collector.
procedure Garbage(o:TObject); overload;

implementation

{$IFDEF RTC_UseGarbageCollector}

var
  CS:TRtcCritSec;
  p_can:array of pointer;
  o_can:array of TObject;
  pcan_cnt:integer=0;
  ocan_cnt:integer=0;
{$ENDIF}

// Add pointer "p" to garbage collector.
procedure Garbage(p:pointer); overload;
  begin
  if p=nil then Exit;
 {$IFDEF RTC_UseGarbageCollector}
    CS.Enter;
    try
      Inc(pcan_cnt);
      if length(p_can)<pcan_cnt then
        SetLength(p_can, length(p_can)+32);
      p_can[pcan_cnt-1]:=p;
    finally
      CS.Leave;
      end;
  {$ELSE}
    FreeMem(p);
  {$ENDIF}
  end;

// Add object "o" to garbage collector.
procedure Garbage(o:TObject); overload;
{$IFDEF RTC_UseGarbageCollector}
  {$IFDEF RTC_DEBUG}
  var
    cname:String;
  {$ENDIF}
{$ENDIF}
  begin
  if o=nil then Exit;
 {$IFDEF RTC_UseGarbageCollector}
    CS.Enter;
    try
      {$IFDEF RTC_DEBUG}
      cname:='?';
      try
        cname:=o.ClassName;
        Log('Garbage('+cname+')','DEBUG');
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('Garbage('+cname+')',E,'ERROR');
        end;
      {$ENDIF}
      Inc(ocan_cnt);
      if length(o_can)<ocan_cnt then
        SetLength(o_can, length(o_can)+32);
      o_can[ocan_cnt-1]:=o;
    finally
      CS.Leave;
      end;
  {$ELSE}
    RtcFreeAndNil(o);
  {$ENDIF}
  end;

{$IFDEF RTC_UseGarbageCollector}
procedure CleanGarbage;
  var
{$IFDEF RTC_DEBUG}s:integer;{$ENDIF}
    a:integer;
    cname:String;
  begin
  CS.Enter;
  try
    {$IFDEF RTC_DEBUG} Log('CleanGarbage begin ('+IntToStr(ocan_cnt)+' + '+IntToStr(pcan_cnt)+') ...','DEBUG'); {$ENDIF}
    for a:=0 to ocan_cnt-1 do
      begin
      cname:='?';
      try
        cname:=o_can[a].ClassName;
        {$IFDEF RTC_DEBUG} Log('Free Object ('+IntToStr(ocan_cnt-a)+': '+cname+')','DEBUG'); {$ENDIF}
        RtcFreeAndNil(o_can[a]);
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('CleanGarbage: Free Object ('+IntToStr(ocan_cnt-a)+': '+cname+')',E,'ERROR');
        end;
      end;
    ocan_cnt:=0;
    for a:=0 to pcan_cnt-1 do
      try
        {$IFDEF RTC_DEBUG}
        s:=SizeOf(p_can[a]^);
        Log('Free Mem ('+IntToStr(pcan_cnt-a)+': '+IntToStr(s)+' byte)','DEBUG');
        {$ENDIF}
        FreeMem(p_can[a]);
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('CleanGarbage: Free Mem ('+IntToStr(pcan_cnt-a)+')',E,'ERROR');
        end;
    pcan_cnt:=0;
    {$IFDEF RTC_DEBUG} Log('CleanGarbage end.','DEBUG'); {$ENDIF}
  finally
    CS.Leave;
    end;

  SetLength(p_can,0);
  SetLength(o_can,0);

  RtcFreeAndNil(CS);
  end;
{$ENDIF}

initialization
{$IFDEF RTC_UseGarbageCollector}
{$IFDEF RTC_DEBUG} Log('rtcTrashcan Initializing ...','DEBUG');{$ENDIF}

CS:=TRtcCritSec.Create;
SetLength(p_can,0);
SetLength(o_can,0);

{$IFDEF RTC_DEBUG} Log('rtcTrashcan Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcTrashcan Finalizing ...','DEBUG');{$ENDIF}

CleanGarbage;

{$IFDEF RTC_DEBUG} Log('rtcTrashcan Finalized.','DEBUG');{$ENDIF}
{$ENDIF}
end.
