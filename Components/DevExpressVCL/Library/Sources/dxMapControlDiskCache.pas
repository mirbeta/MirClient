{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxMapControlDiskCache;

interface

{$I cxVer.inc}

uses
  Windows, SysUtils, StrUtils, Math, Classes, RTLConsts, DateUtils,
  dxCore, dxCoreClasses, dxGdiPlusClasses,
  dxMapControlTypes;

const
  CacheFileExtension = '.xtramapcache';
  CacheMapFileName = 'dxcache.map';

type
  TdxMapControlDiskCache = class;

  TdxMapControlCacheMapItem = class
  private
    FName: string;
    FSize: Integer;
    FTime: TDateTime;
  public
    constructor Create(ARecord: string); overload;
    constructor Create(const AName: string; ASize: Integer; ATime: TDateTime); overload;
    function GetAsText: string;
    property Name: string read FName;
    property Size: Integer read FSize;
    property Time: TDateTime read FTime write FTime;
  end;

  TdxMapControlDiskCacheMap = class
  private
    FCache: TdxMapControlDiskCache;
    FItems: TdxFastObjectList;
    FSize: Int64;
    function GetCount: Integer;
    function GetFullFileName: string;
    function GetItems(Index: Integer): TdxMapControlCacheMapItem;
    procedure LoadItems;
    procedure SaveItems;
  public
    constructor Create(ACache: TdxMapControlDiskCache); virtual;
    destructor Destroy; override;
    function GetItem(const AFileName: string): TdxMapControlCacheMapItem;
    procedure PushItem(const AFileName: string; ASize: Int64);
    procedure RemoveItem(AItem: TdxMapControlCacheMapItem);
    procedure UpdateItem(AItem: TdxMapControlCacheMapItem);

    property Count: Integer read GetCount;
    property Items [Index: Integer]: TdxMapControlCacheMapItem read GetItems;
    property Size: Int64 read FSize;
  end;

  TdxMapControlDiskCache = class
  private
    FCacheMap: TdxMapControlDiskCacheMap;
    FDiskLimit: Integer;
    FExpireTime: Int64;
    FRoot: string;
    function GetItemName(AIndex: TdxMapControlTileIndex;
      const APrefix: string): string;
  protected
    procedure CheckExpiredFiles;
    procedure CheckWatchdog;
    procedure ProcessCache;
    procedure SaveToDisk;
    procedure Watchdog;
    property Root: string read FRoot;
  public
    constructor Create(AFolder: string; AExpireTime: Int64; ADiskLimit: Integer);
    destructor Destroy; override;
    function LocatedAt(AFolder: string): Boolean;
    procedure Push(AImage: TdxSmartImage; AIndex: TdxMapControlTileIndex;
      const APrefix: string);
    function TryRetrieveFileName(AIndex: TdxMapControlTileIndex; const APrefix: string; out AFullFileName: string): Boolean;

    property ExpireTime: Int64 read FExpireTime write FExpireTime;
    property DiskLimit: Integer read FDiskLimit write FDiskLimit;
  end;

implementation

uses
  dxMapControlMultiScaleTile;

{ TdxMapControlDiskCacheMap }

constructor TdxMapControlDiskCacheMap.Create(ACache: TdxMapControlDiskCache);
begin
  inherited Create;
  FCache := ACache;
  FItems := TdxFastObjectList.Create;
  if FileExists(GetFullFileName) then
    LoadItems;
end;

destructor TdxMapControlDiskCacheMap.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TdxMapControlDiskCacheMap.GetItem(
  const AFileName: string): TdxMapControlCacheMapItem;
var
  I: Integer;
  AItem: TdxMapControlCacheMapItem;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    AItem := FItems[I] as TdxMapControlCacheMapItem;
    if SameText(AItem.Name, AFileName) then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

procedure TdxMapControlDiskCacheMap.PushItem(const AFileName: string; ASize: Int64);
var
  AItem: TdxMapControlCacheMapItem;
begin
  AItem := TdxMapControlCacheMapItem.Create(AFileName, ASize, Now);
  FItems.Insert(0, AItem);
  Inc(FSize, ASize);
  SaveItems;
end;

procedure TdxMapControlDiskCacheMap.RemoveItem(AItem: TdxMapControlCacheMapItem);
begin
  Dec(FSize, AItem.Size);
  FItems.Remove(AItem);
end;

procedure TdxMapControlDiskCacheMap.UpdateItem(
  AItem: TdxMapControlCacheMapItem);
begin
  AItem.Time := Now;
end;

function TdxMapControlDiskCacheMap.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxMapControlDiskCacheMap.GetFullFileName: string;
begin
  Result := FCache.Root + CacheMapFileName;
end;

function TdxMapControlDiskCacheMap.GetItems(
  Index: Integer): TdxMapControlCacheMapItem;
begin
  Result := FItems[Index] as TdxMapControlCacheMapItem;
end;

procedure TdxMapControlDiskCacheMap.LoadItems;
var
  ARecords: TStringList;
  AItem: TdxMapControlCacheMapItem;
  I: Integer;
begin
  ARecords := TStringList.Create;
  try
    ARecords.LoadFromFile(GetFullFileName);
    for I := 0 to ARecords.Count - 1 do
    begin
      AItem := TdxMapControlCacheMapItem.Create(ARecords[I]);
      FItems.Add(AItem);
      Inc(FSize, AItem.Size);
    end;
  finally
    ARecords.Free;
  end;
end;

procedure TdxMapControlDiskCacheMap.SaveItems;
var
  ARecords: TStringList;
  I: Integer;
begin
  ARecords := TStringList.Create;
  try
    for I := 0 to FItems.Count - 1 do
      ARecords.Add((FItems[I] as TdxMapControlCacheMapItem).GetAsText);
    ARecords.SaveToFile(GetFullFileName);
  finally
    ARecords.Free;
  end;
end;

{ TdxMapControlDiskCache }

constructor TdxMapControlDiskCache.Create(AFolder: string;
  AExpireTime: Int64; ADiskLimit: Integer);
begin
  inherited Create;
  FRoot := IncludeTrailingPathDelimiter(AFolder);
  ForceDirectories(FRoot);
  FCacheMap := TdxMapControlDiskCacheMap.Create(Self);
  FExpireTime := AExpireTime;
  FDiskLimit := ADiskLimit;

//    FWatchdogLocker = new object();
//    FWatchdogEvent = new ManualResetEventSlim(true);
//    FWatchdogStartedEvent = new ManualResetEventSlim(false);

   // ThreadPool.QueueUserWorkItem(new WaitCallback(Watchdog));
//    FWatchdogStartedEvent.Wait();
//    FWatchdogStartedEvent.Dispose();
//    FWatchdogStartedEvent := nil;
end;

destructor TdxMapControlDiskCache.Destroy;
begin
  FreeAndNil(FCacheMap);
  inherited;
end;

function TdxMapControlDiskCache.LocatedAt(AFolder: string): Boolean;
begin
  Result := SameFileName(FRoot, AFolder);
end;

procedure TdxMapControlDiskCache.Push(AImage: TdxSmartImage;
  AIndex: TdxMapControlTileIndex; const APrefix: string);
var
  AFileStream: TFileStream;
  ARelativeFileName: string;
begin
  if FRoot <> '' then
  begin
    CheckExpiredFiles;
    ARelativeFileName := GetItemName(AIndex, APrefix);
    AFileStream := TFileStream.Create(Root + ARelativeFileName, fmCreate);
    try
      AImage.SaveToStream(AFileStream);
      FCacheMap.PushItem(ARelativeFileName, AFileStream.Size);
    finally
      AFileStream.Free;
    end;
  end;
end;

function TdxMapControlDiskCache.TryRetrieveFileName(AIndex: TdxMapControlTileIndex;
  const APrefix: string; out AFullFileName: string): Boolean;
var
  AFileName: string;
  ACacheMapItem: TdxMapControlCacheMapItem;
  AFileDateTime: TDateTime;
  AExpireTime: Int64;
begin
  Result := False;
  AFileName := GetItemName(AIndex, APrefix);
  ACacheMapItem := FCacheMap.GetItem(AFileName);
  if ACacheMapItem <> nil then
  begin
    AFullFileName := Root + AFileName;
    if FileAge(AFullFileName, AFileDateTime) then
    begin
      AExpireTime := MilliSecondsBetween(Now, AFileDateTime);
      if not SameValue(ExpireTime, 0) and (AExpireTime >= ExpireTime) then
      begin
        DeleteFile(AFullFileName);
        FCacheMap.RemoveItem(ACacheMapItem);
        Exit;
      end;
      FCacheMap.UpdateItem(ACacheMapItem);
      Result := True;
    end
    else
      FCacheMap.RemoveItem(ACacheMapItem);
  end;
end;

procedure TdxMapControlDiskCache.CheckExpiredFiles;
begin
  ProcessCache;
end;

procedure TdxMapControlDiskCache.CheckWatchdog;
begin

end;

procedure TdxMapControlDiskCache.ProcessCache;
var
  I: Integer;
  ASize: Int64;
  ADiskLimitInBytes: Int64;
begin
  if (Root <> '') and (DiskLimit > 0) then
  begin
    ASize := FCacheMap.Size;
    ADiskLimitInBytes := DiskLimit;
    ADiskLimitInBytes := ADiskLimitInBytes shl 20;
    if ASize > ADiskLimitInBytes then
      for I := FCacheMap.Count - 1 downto 0 do
      begin
        Dec(ASize, FCacheMap.Items[I].Size);
        DeleteFile(Root + FCacheMap.Items[I].Name);
        FCacheMap.RemoveItem(FCacheMap.Items[I]);
        if ASize < ADiskLimitInBytes then
          Break;
      end;
  end;
end;

procedure TdxMapControlDiskCache.SaveToDisk;
begin

end;

procedure TdxMapControlDiskCache.Watchdog;
begin

end;

function TdxMapControlDiskCache.GetItemName(AIndex: TdxMapControlTileIndex;
  const APrefix: string): string;
begin
  Result := Format('%s_%d_%d_%d%s', [APrefix,
    AIndex.X, AIndex.Y, AIndex.Level, CacheFileExtension]);
end;

{ TdxMapControlCacheMapItem }

constructor TdxMapControlCacheMapItem.Create(ARecord: string);
var
  APos, APos1: Integer;
  AName: string;
  ATime: TDateTime;
  ASize: Integer;
begin
  APos := Pos(':', ARecord);
  AName := Copy(ARecord, 1, APos - 1);
  APos1 := PosEx(':', ARecord, APos + 1);
  ATime := dxStrToFloat(Copy(ARecord, APos + 1, APos1 - APos - 1));
  ASize := StrToInt(Copy(ARecord, APos1 + 1, Length(ARecord)));
  Create(AName, ASize, ATime);
end;

constructor TdxMapControlCacheMapItem.Create(const AName: string;
  ASize: Integer; ATime: TDateTime);
begin
  inherited Create;
  FName := AName;
  FSize := ASize;
  FTime := ATime;
end;

function TdxMapControlCacheMapItem.GetAsText: string;
begin
  Result := FName + ':' +
    dxFloatToStr(FTime) + ':' +
    IntToStr(FSize) ;
end;

end.
