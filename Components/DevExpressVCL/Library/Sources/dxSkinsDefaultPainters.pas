{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSkins Library                                     }
{                                                                    }
{           Copyright (c) 2006-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING     }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxSkinsDefaultPainters;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, dxGDIPlusApi, cxLookAndFeelPainters, dxSkinsCore,
  dxSkinsLookAndFeelPainter, dxSkinsStrs;

const
  sdxDefaultUserSkinData = 'DefaultUserSkinData';
  sdxSkinsUserSkinName = 'UserSkin';

type

  { TdxSkinsUserSkinPainter }

  TdxSkinsUserSkinPainter = class(TdxSkinLookAndFeelPainter)
  protected
    function GetSkinInfoClass: TdxSkinLookAndFeelPainterInfoClass; override;
  public
    function IsInternalPainter: Boolean; override;
    function LookAndFeelName: string; override;
  end;

procedure dxSkinsPopulateSkinResources(AModule: HMODULE; AResNames, ASkinNames: TStringList);
function dxSkinsLoadedUserSkinHasMissingElements: Boolean;
function dxSkinsUserSkinGetLoadedSkinName(ADisplayName: Boolean = False): string;
function dxSkinsUserSkinLoadFromFile(const AFileName: string; const ASkinName: string = ''): Boolean;
function dxSkinsUserSkinLoadFromFileByIndex(const AFileName: string; ASkinIndex: Integer = 0): Boolean;
function dxSkinsUserSkinLoadFromResource(AInstance: THandle;
  const AResourceName: string; const ASkinName: string = ''): Boolean;
function dxSkinsUserSkinLoadFromStream(AStream: TStream; const ASkinName: string = ''): Boolean;
function dxSkinsUserSkinLoadFromStreamByIndex(AStream: TStream; ASkinIndex: Integer = 0): Boolean;
function dxSkinsUserSkinPopulateSkinNames(AStream: TStream;
  AList: TStrings; ADisplayNames: Boolean = False): Boolean; overload;
function dxSkinsUserSkinPopulateSkinNames(const AFileName: string;
  AList: TStrings; ADisplayNames: Boolean = False): Boolean; overload;
implementation

uses
  dxCore, dxSkinInfo, cxLookAndFeels, Math;

{$R dxSkinsDefaultPainters.res}

type

  { TdxSkinUserLookAndFeelPainterInfo }

  TdxSkinUserLookAndFeelPainterInfo = class(TdxSkinLookAndFeelPainterInfo)
  protected
    procedure SkinChanged(Sender: TdxSkin); override;
  end;

  { TdxListPair }

  TdxListPair = class(TObject)
  public
    ResNames, SkinNames: TStringList;
  end;

function ReadStringFromStream(AStream: TStream): string;
var
  L: Integer;
  ATemp: AnsiString;
begin
  AStream.Read(L, SizeOf(L));
  SetLength(ATemp, L);
  if L > 0 then
    AStream.ReadBuffer(ATemp[1], L);
  Result := dxAnsiStringToString(ATemp);
end;

function EnumResNameProc(hModule: HMODULE; lpszType: LPCTSTR;
  lpszName: LPTSTR; AData: TdxListPair): Boolean; stdcall;

  function IsSkinResource(var ASkinName: string): Boolean;
  var
    AStream: TStream;
    AVersion: Double;
  begin
    AStream := TResourceStream.Create(hModule, lpszName, lpszType);
    try
      Result := dxSkinCheckSignature(AStream, AVersion);
      if Result then
      begin
        ASkinName := ReadStringFromStream(AStream);
        Result := Result and
          (AData.ResNames.IndexOf(lpszName) = -1) and
          (AData.SkinNames.IndexOf(ASkinName) = -1);
      end;
    finally
      AStream.Free;
    end;
  end;

var
  ASkinName: string;
begin
  Result := True;
  if IsSkinResource(ASkinName) then
  begin
    AData.SkinNames.AddObject(ASkinName,
      TObject(AData.ResNames.AddObject(lpszName, TObject(hModule))));
  end;
end;

procedure dxSkinsPopulateSkinResources(AModule: HMODULE; AResNames, ASkinNames: TStringList);
var
  AData: TdxListPair;
begin
  AResNames.Clear;
  ASkinNames.Clear;
  AData := TdxListPair.Create;
  try
    AData.ResNames := AResNames;
    AData.SkinNames := ASkinNames;
    Windows.EnumResourceNames(AModule, PChar(sdxResourceType), @EnumResNameProc, LPARAM(AData));
    ASkinNames.Sort;
  finally
    AData.Free;
  end;
end;

function dxSkinsGetUserSkin(out ASkin: TdxSkin): Boolean;
var
  APainter: TcxCustomLookAndFeelPainter;
  ASkinInfo: TdxSkinLookAndFeelPainterInfo;
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(sdxSkinsUserSkinName, APainter) and
    APainter.GetPainterData(ASkinInfo);
  if Result then
    ASkin := ASkinInfo.Skin;
end;

function dxSkinsLoadedUserSkinHasMissingElements: Boolean;
var
  ASkin: TdxSkin;
begin
  Result := dxSkinsGetUserSkin(ASkin) and ASkin.HasMissingElements;
end;

function dxSkinsUserSkinGetLoadedSkinName(ADisplayName: Boolean = False): string;
var
  ASkin: TdxSkin;
begin
  if not dxSkinsGetUserSkin(ASkin) then
    Result := ''
  else
    if ADisplayName then
      Result := ASkin.DisplayName
    else
      Result := ASkin.Name;
end;

function dxSkinsUserSkinLoadFromFile(
  const AFileName: string; const ASkinName: string = ''): Boolean;
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := dxSkinsUserSkinLoadFromStream(AFileStream, ASkinName);
  finally
    AFileStream.Free;
  end;
end;

function dxSkinsUserSkinLoadFromFileByIndex(const AFileName: string; ASkinIndex: Integer = 0): Boolean;
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := dxSkinsUserSkinLoadFromStreamByIndex(AFileStream, ASkinIndex);
  finally
    AFileStream.Free;
  end;
end;

function dxSkinsUserSkinLoadFromResource(AInstance: THandle;
  const AResourceName: string; const ASkinName: string = ''): Boolean;
var
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(AInstance, AResourceName, sdxResourceType);
  try
    Result := dxSkinsUserSkinLoadFromStream(AStream, ASkinName);
  finally
    AStream.Free;
  end;
end;

function dxSkinsUserSkinLoadFromStream(AStream: TStream; const ASkinName: string = ''): Boolean;
var
  AReader: TdxSkinBinaryReader;
  ASkin: TdxSkin;
begin
  Result := dxSkinsGetUserSkin(ASkin);
  if Result then
  begin
    AReader := TdxSkinBinaryReader.Create(AStream);
    try
      Result := AReader.LoadSkin(ASkin, ASkinName);
    finally
      AReader.Free;
    end;
  end;
end;

function dxSkinsUserSkinLoadFromStreamByIndex(AStream: TStream; ASkinIndex: Integer = 0): Boolean;
var
  AReader: TdxSkinBinaryReader;
  ASkin: TdxSkin;
begin
  Result := dxSkinsGetUserSkin(ASkin);
  if Result then
  begin
    AReader := TdxSkinBinaryReader.Create(AStream);
    try
      Result := AReader.LoadSkin(ASkin, ASkinIndex);
    finally
      AReader.Free;
    end;
  end;
end;

function dxSkinsUserSkinPopulateSkinNames(
  AStream: TStream; AList: TStrings; ADisplayNames: Boolean = False): Boolean;
var
  AReader: TdxSkinBinaryReader;
  I: Integer;
begin
  AReader := TdxSkinBinaryReader.Create(AStream);
  try
    Result := AReader.Count > 0;
    if Result then
    begin
      AList.Capacity := Max(AList.Capacity, AList.Count + AReader.Count);
      for I := 0 to AReader.Count - 1 do
      begin
        if ADisplayNames then
          AList.Add(AReader.SkinDisplayName[I])
        else
          AList.Add(AReader.SkinName[I]);
      end;
    end;
  finally
    AReader.Free;
  end;
end;

function dxSkinsUserSkinPopulateSkinNames(const AFileName: string;
  AList: TStrings; ADisplayNames: Boolean = False): Boolean;
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := dxSkinsUserSkinPopulateSkinNames(AFileStream, AList, ADisplayNames);
  finally
    AFileStream.Free;
  end;
end;

{ TdxSkinsUserSkinPainter }

function TdxSkinsUserSkinPainter.GetSkinInfoClass: TdxSkinLookAndFeelPainterInfoClass;
begin
  Result := TdxSkinUserLookAndFeelPainterInfo;
end;

function TdxSkinsUserSkinPainter.IsInternalPainter: Boolean;
begin
  Result := True;
end;

function TdxSkinsUserSkinPainter.LookAndFeelName: string;
begin
  Result := sdxSkinsUserSkinName;
end;

{  TdxSkinUserLookAndFeelPainterInfo }

procedure TdxSkinUserLookAndFeelPainterInfo.SkinChanged(Sender: TdxSkin);
begin
  inherited SkinChanged(Sender);
  RootLookAndFeel.Refresh;
end;

//

procedure RegisterPainters;
begin
  if CheckGdiPlus then
    cxLookAndFeelPaintersManager.Register(
      TdxSkinsUserSkinPainter.Create(sdxDefaultUserSkinData, HInstance));
end;

procedure UnregisterPainters;
begin
  if cxLookAndFeelPaintersManager <> nil then
    cxLookAndFeelPaintersManager.UnRegister(sdxSkinsUserSkinName);
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterPainters, @UnregisterPainters);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterPainters);

end.
