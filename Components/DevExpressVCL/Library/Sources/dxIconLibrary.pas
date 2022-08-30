{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxIconLibrary;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
  Classes, Windows, SysUtils, Controls, Generics.Defaults, Generics.Collections,
  cxClasses, dxCore, cxGraphics, dxSmartImage, cxGeometry, dxGDIPlusClasses, dxHash, dxHashUtils;

type
  TdxIconLibraryCustomObject = class;
  TdxIconLibrary = class;
  TdxIconLibraryItemClass = class of TdxIconLibraryCustomObject;

  TSearchAttribute = (saFile, saDirectory);
  TSearchAttributes = set of TSearchAttribute;

  { TdxIconLibraryCustomObject }

  TdxIconLibraryCustomObject = class
  private
    FDisplayName: string;
    FName: string;
  public
    constructor Create(const AName: string); virtual;
    procedure Populate; virtual; abstract;

    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
  end;
  TdxIconLibraryCustomObjectClass = class of TdxIconLibraryCustomObject;

  { TdxIconLibraryCollection }

  TdxIconLibraryCollection = class(TdxIconLibraryCustomObject)
  private
    FItemClass: TdxIconLibraryCustomObjectClass;
    FItems: TcxObjectList;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxIconLibraryCustomObject;
  protected
    procedure GetFiles(AFiles: TStrings); virtual; abstract;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    function Add(const AName: string): TdxIconLibraryCustomObject;
    procedure Remove(AItem: TdxIconLibraryCustomObject);
    procedure SortByName;
    procedure Populate; override;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxIconLibraryCustomObject read GetItem; default;
  end;

  { TdxIconLibraryImage }

  TdxIconLibraryImage = class(TdxIconLibraryCustomObject)
  private
    FHeight: Integer;
    FImage: TdxSmartGlyph;
    FImageSize: string;
    FTag: TcxTag;
    FWidth: Integer;

    function GetFileName: string;
    function GetImageSize: string;
    function SizeToText: string;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    procedure Populate; override;
    procedure LoadFromFile;
    function IsFileExists: Boolean;

    property FileName: string read GetFileName;
    property Height: Integer read FHeight;
    property Image: TdxSmartGlyph read FImage;
    property ImageSize: string read GetImageSize;
    property Tag: TcxTag read FTag write FTag;
    property Width: Integer read FWidth;
  end;

 { TdxIconLibraryCategory }

  TdxIconLibraryCategory = class(TdxIconLibraryCollection)
  private
    function GetItem(AIndex: Integer): TdxIconLibraryImage;
  protected
    procedure GetFiles(AFiles: TStrings); override;
  public
    constructor Create(const AName: string); override;

    property Items[Index: Integer]: TdxIconLibraryImage read GetItem; default;
  end;

  { TdxIconLibrarySet }

  TdxIconLibrarySet = class(TdxIconLibraryCollection)
  private
    function GetItem(AIndex: Integer): TdxIconLibraryCategory;
  protected
    procedure GetFiles(AFiles: TStrings); override;
  public
    constructor Create(const AName: string); override;

    property Items[Index: Integer]: TdxIconLibraryCategory read GetItem; default;
  end;

  { TdxIconLibrary }

  TdxIconLibrary = class(TdxIconLibraryCollection)
  private
    function GetItem(AIndex: Integer): TdxIconLibrarySet;
  protected
    procedure GetFiles(AFiles: TStrings); override;
  public
    constructor Create(const AName: string); override;
    property Items[Index: Integer]: TdxIconLibrarySet read GetItem; default;
  end;

function dxGetIconLibraryPath: string;
procedure GetFileList(APath: string; AFileNames: TStrings;
  ASearchAttributes: TSearchAttributes; AAddPathToFileName, ARecursive: Boolean);

implementation

uses
  Types;

const
  dxIconLibraryRelativePath = '\ExpressLibrary\Sources\Icon Library\';

function dxGetIconLibraryPath: string;
begin
  Result := GetEnvironmentVariable('DXVCL') + dxIconLibraryRelativePath;
end;

procedure GetFileList(APath: string; AFileNames: TStrings;
  ASearchAttributes: TSearchAttributes; AAddPathToFileName, ARecursive: Boolean);
var
  AFilePath: string;
  AHandle: THandle;
  AFindData: TWIN32FindData;

  function IsDirectory(AFindData: TWIN32FindData): Boolean;
  var
    AFileName: string;
  begin
    AFileName := AFindData.cFileName;
    Result := (AFileName <> '.') and (AFileName <> '..') and
      (AFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0);
  end;

  function IsFile(AFindData: TWIN32FindData): Boolean;
  var
    AFileName: string;
  begin
    AFileName := AFindData.cFileName;
    Result := (AFileName <> '.') and (AFileName <> '..') and
      (AFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0);
  end;

  function IsCorrectFileName(AFindData: TWIN32FindData): Boolean;
  begin
    Result :=
      ((saFile in ASearchAttributes) and IsFile(AFindData)) or
      ((saDirectory in ASearchAttributes) and IsDirectory(AFindData));
  end;

  function GetFileName(AFindData: TWIN32FindData): string;
  begin
    Result := AFindData.cFileName;
    if AAddPathToFileName then
      Result := AFilePath + Result;
  end;

var
  AMask: string;
begin
  if DirectoryExists(APath) then
  begin
    AFilePath := IncludeTrailingPathDelimiter(APath);
    APath := AFilePath + '*';
    AMask := '*';
  end
  else
  begin
    AFilePath := ExtractFilePath(APath);
    AMask := Copy(APath, Length(AFilePath) + 1, MaxInt);
    if ARecursive then
      APath := AFilePath + '*';
  end;
  AHandle := FindFirstFile(PChar(APath), AFindData);
  if AHandle <> INVALID_HANDLE_VALUE then
    try
      repeat
        if ARecursive and IsDirectory(AFindData) then
          GetFileList(AFilePath + AFindData.cFileName + '\' + AMask, AFileNames,
            ASearchAttributes, AAddPathToFileName, ARecursive);
        if IsCorrectFileName(AFindData) and ((AMask = '*') or SameText(ExtractFileExt(AFindData.cFileName), ExtractFileExt(AMask))) then
          AFileNames.AddObject(GetFileName(AFindData), TObject(AFindData.nFileSizeLow));
      until not FindNextFile(AHandle, AFindData);
    finally
      Windows.FindClose(AHandle);
    end;
end;

function CompareItemsProc(AItem1, AItem2: TdxIconLibraryCustomObject): Integer;
begin
  Result := CompareStr(AItem1.Name, AItem2.Name);
end;

{ TdxIconLibraryCustomObject }

constructor TdxIconLibraryCustomObject.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FDisplayName := ExtractFileName(FName);
end;

{ TdxIconLibraryCollection }

constructor TdxIconLibraryCollection.Create(const AName: string);
begin
  inherited;
  FItems := TcxObjectList.Create;
  FItemClass := TdxIconLibraryCustomObject;
end;

destructor TdxIconLibraryCollection.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TdxIconLibraryCollection.Add(const AName: string): TdxIconLibraryCustomObject;
begin
  Result := FItemClass.Create(AName);
  FItems.Add(Result);
end;

procedure TdxIconLibraryCollection.Remove(AItem: TdxIconLibraryCustomObject);
begin
  FItems.Remove(AItem);
end;

procedure TdxIconLibraryCollection.SortByName;
begin
  FItems.Sort(@CompareItemsProc);
end;

procedure TdxIconLibraryCollection.Populate;
var
  I: Integer;
  AFiles: TStrings;
begin
  if DirectoryExists(Name) then
  begin
    AFiles := TStringList.Create;
    try
      GetFiles(AFiles);
      for I := 0 to AFiles.Count - 1 do
        Add(AFiles[I]).Populate;
    finally
      AFiles.Free;
    end;
    SortByName;
  end;
end;

function TdxIconLibraryCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxIconLibraryCollection.GetItem(AIndex: Integer): TdxIconLibraryCustomObject;
begin
  Result := TdxIconLibraryCustomObject(FItems[AIndex]);
end;

{ TdxIconLibraryImage }

constructor TdxIconLibraryImage.Create(const AName: string);
begin
  inherited;
  FImage := TdxSmartGlyph.Create;
end;

destructor TdxIconLibraryImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TdxIconLibraryImage.IsFileExists: Boolean;
begin
  Result := FileExists(FileName);
end;

procedure TdxIconLibraryImage.Populate;
var
  ACodec: TdxSmartImageCodecClass;
  AImageSize: TSize;
begin
  if not TdxSmartImageCodecsRepository.GetImageInfo(FileName, AImageSize, ACodec) then
    AImageSize := cxNullSize;
  FWidth := AImageSize.cx;
  FHeight := AImageSize.cy;
end;

procedure TdxIconLibraryImage.LoadFromFile;
begin
  FImage.LoadFromFile(FileName);
end;

function TdxIconLibraryImage.GetFileName: string;
begin
  Result := Name;
end;

function TdxIconLibraryImage.GetImageSize: string;
begin
  if FImageSize = '' then
    FImageSize := SizeToText;
  Result := FImageSize;
end;

function TdxIconLibraryImage.SizeToText: string;
begin
  if SameText(ExtractFileExt(FileName), '.svg') then
    Result := 'Vector'
  else
    Result := IntToStr(Width) + 'x' + IntToStr(Height);
end;

{ TdxIconLibraryCategory }

constructor TdxIconLibraryCategory.Create(const AName: string);
begin
  inherited;
  FItemClass := TdxIconLibraryImage;
end;

procedure TdxIconLibraryCategory.GetFiles(AFiles: TStrings);
begin
  GetFileList(Name, AFiles, [saFile], True, True);
end;

function TdxIconLibraryCategory.GetItem(AIndex: Integer): TdxIconLibraryImage;
begin
  Result := inherited Items[AIndex] as TdxIconLibraryImage;
end;

{ TdxIconLibrarySet }

constructor TdxIconLibrarySet.Create(const AName: string);
begin
  inherited;
  FItemClass := TdxIconLibraryCategory;
end;

procedure TdxIconLibrarySet.GetFiles(AFiles: TStrings);
begin
  GetFileList(Name, AFiles, [saDirectory], True, False);
end;

function TdxIconLibrarySet.GetItem(AIndex: Integer): TdxIconLibraryCategory;
begin
  Result := inherited Items[AIndex] as TdxIconLibraryCategory;
end;

{ TdxIconLibrary }

constructor TdxIconLibrary.Create(const AName: string);
begin
  if AName = '' then
    inherited Create(dxGetIconLibraryPath)
  else
    inherited;
  FItemClass := TdxIconLibrarySet;
end;

procedure TdxIconLibrary.GetFiles(AFiles: TStrings);
begin
  GetFileList(Name, AFiles, [saDirectory], True, False);
end;

function TdxIconLibrary.GetItem(AIndex: Integer): TdxIconLibrarySet;
begin
  Result := inherited Items[AIndex] as TdxIconLibrarySet;
end;

end.
