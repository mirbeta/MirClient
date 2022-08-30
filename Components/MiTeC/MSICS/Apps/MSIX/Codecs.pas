{$INCLUDE ..\..\Compilers.Inc}

unit Codecs;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

procedure CompressStream(InStream, OutStream: TStream);
procedure DecompressStream(InStream, OutStream: TStream);

implementation

uses {$IFDEF RAD9PLUS}
     System.ZLib;
     {$ELSE}
     ZLib;
     {$ENDIF}

procedure CompressStream(InStream, OutStream: TStream);
{$IFNDEF RAD7PLUS}
var
  InpBuf, OutBuf: Pointer;
  InpBytes, OutBytes: Integer;
begin
  InpBuf:=nil;
  OutBuf:=nil;
  try
    GetMem(InpBuf,InStream.Size);
    InStream.Position:=0;
    InpBytes:=InStream.Read(InpBuf^,InStream.Size);
    CompressBuf(InpBuf, InpBytes,OutBuf,OutBytes);
    OutStream.Write(OutBuf^,OutBytes);
  finally
    if InpBuf<>nil then
      FreeMem(InpBuf);
    if OutBuf<>nil then
      FreeMem(OutBuf);
  end;
{$ELSE}
begin
  InStream.Position:=0;
  ZCompressStream(InStream, OutStream, zcMax);
{$ENDIF}
 OutStream.Position:=0;
end;

procedure DecompressStream(InStream, OutStream: TStream);
{$IFNDEF RAD7PLUS}
var
  InpBuf, OutBuf: Pointer;
  OutBytes, Size: Integer;
begin
  InStream.Position:=0;
  InpBuf:=nil;
  OutBuf:=nil;
  Size:=InStream.Size-InStream.Position;
  if Size>0 then
    try
      GetMem(InpBuf,Size);
      InStream.Read(InpBuf^,Size);
      DecompressBuf(InpBuf,Size,0,OutBuf, OutBytes);
      OutStream.Write(OutBuf^,OutBytes);
    finally
      if InpBuf<>nil then
        FreeMem(InpBuf);
      if OutBuf<>nil then
        FreeMem(OutBuf);
    end;
  OutStream.Position:=0;
{$ELSE}
begin
  InStream.Position:=0;
  ZDecompressStream(InStream, OutStream);
  OutStream.Position:=0;
{$ENDIF}
end;


end.
