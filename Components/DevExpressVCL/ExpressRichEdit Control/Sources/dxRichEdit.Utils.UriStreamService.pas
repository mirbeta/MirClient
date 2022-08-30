{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Utils.UriStreamService;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCoreClasses, dxThreading,
  dxRichEdit.Utils.OfficeImage;

type
  IdxUriStreamProvider = interface
  ['{BBD4B8A8-C6F1-4FCA-971C-6A4963BE20CB}']
    function GetStream(const AUri: string): TStream;
    function IsAsyncLoadingSupported(const AUri: string): Boolean;
  end;

  IdxUriStreamProvider2 = interface(IdxUriStreamProvider)
  ['{C060940C-6100-48B7-8DA3-B2624C996CD8}']
    function AsyncGetStream(const AUri: string; const ACancelStatus: TdxTaskCancelCallback): TStream;
  end;

  IdxUriStreamService = interface
  ['{079683F3-DB6C-4DE9-A0AE-8EBB45900199}']
    function GetStream(const AUri: string): TStream;
    procedure RegisterProvider(const AProvider: IdxUriStreamProvider);
    procedure UnregisterProvider(const AProvider: IdxUriStreamProvider);

    function IsAsyncLoadingSupported(const AUri: string): Boolean;
  end;

  IdxUriStreamService2 = interface(IdxUriStreamService)
  ['{B8FA7B72-CEE8-411C-8295-E494A82BE126}']
    function AsyncGetStream(const AUri: string; const ACancelStatus: TdxTaskCancelCallback): TStream;
  end;

  TdxUriStreamProviderCollection = TList<IdxUriStreamProvider>;

  { TdxUriStreamService }

  TdxUriStreamService = class(TInterfacedObject, IdxUriStreamService, IdxUriStreamService2)
  strict private
    FProviders: TdxUriStreamProviderCollection;
  protected
    procedure RegisterDefaultProviders; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // IdxUriStreamService
    function GetStream(const AUri: string): TStream;
    procedure RegisterProvider(const AProvider: IdxUriStreamProvider);
    procedure UnregisterProvider(const AProvider: IdxUriStreamProvider);
    function IsAsyncLoadingSupported(const AUri: string): Boolean;
    // IdxUriStreamService2
    function AsyncGetStream(const AUri: string; const ACancelStatus: TdxTaskCancelCallback): TStream;

    property Providers: TdxUriStreamProviderCollection read FProviders;
  end;

  { TdxLocalUriStreamProvider }

  TdxLocalUriStreamProvider = class(TInterfacedObject, IdxUriStreamProvider)
  protected
    //IdxUriStreamProvider
    function GetStream(const AUri: string): TStream;
    function IsAsyncLoadingSupported(const AUrl: string): Boolean;
  end;

  { TdxWebUriStreamProvider }

  TdxWebUriStreamProvider = class(TInterfacedObject, IdxUriStreamProvider, IdxUriStreamProvider2)
  protected
    //IdxUriStreamProvider
    function GetStream(const AUri: string): TStream;
    function IsAsyncLoadingSupported(const AUrl: string): Boolean;
    //IdxUriStreamProvider2
    function AsyncGetStream(const AUri: string; const ACancelStatus: TdxTaskCancelCallback): TStream;
  end;

  { TdxDataStringUriStreamProvider }

  TdxDataStringUriStreamProvider = class(TInterfacedObject, IdxUriStreamProvider)
  protected
    //IdxUriStreamProvider
    function GetStream(const AUri: string): TStream;
    function IsAsyncLoadingSupported(const AUrl: string): Boolean;
  end;

  { IdxUriProvider }

  IdxUriProvider = interface
  ['{CD8801AC-509C-4864-96E3-6C446B989E60}']
    function CreateImageUri(const ARootUri: string; AImage: TdxOfficeImage; const ARelativeUri: string): string;
    function CreateCssUri(const ARootUri, AStyleText, ARelativeUri: string): string;
  end;

  { IdxUriProviderService }

  IdxUriProviderService = interface
  ['{D3E18184-253D-4D93-8869-56B5EB31D804}']
    function CreateImageUri(const ARootUri: string; AImage: TdxOfficeImage; const ARelativeUri: string): string;
    function CreateCssUri(const ARootUri, AStyleText, ARelativeUri: string): string;
    procedure RegisterProvider(const AProvider: IdxUriProvider);
    procedure UnregisterProvider(const AProvider: IdxUriProvider);
  end;

  TdxUriProviderCollection = TList<IdxUriProvider>;

  { TdxUriProviderService }

  TdxUriProviderService = class(TInterfacedObject, IdxUriProviderService)
  strict private
    FProviders: TdxUriProviderCollection;
  protected
    procedure RegisterDefaultProviders; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // IdxUriProviderService
    function CreateImageUri(const ARootUri: string; AImage: TdxOfficeImage; const ARelativeUri: string): string;
    function CreateCssUri(const ARootUri, AStyleText, ARelativeUri: string): string;
    procedure RegisterProvider(const AProvider: IdxUriProvider);
    procedure UnregisterProvider(const AProvider: IdxUriProvider);

    property Providers: TdxUriProviderCollection read FProviders;
  end;

  { TdxFileBasedUriProvider }

  TdxFileBasedUriProvider = class(TInterfacedObject, IdxUriProvider)
  strict private const
    FileUriPrefix = 'file:///';
  strict private
    FLastPath: string;
    FLastName: string;
    FLastExtension: string;
    FLastFileIndex: Integer;
  protected
    function GetRelativeFileName(const ARootUri, ARelativeUri, AFileName: string): string;
    function TryToSaveImageInNativeFormat(AImage: TdxOfficeImage; const AFileName: string): Boolean; virtual;
    function TryToSaveImageAsPng(AImage: TdxOfficeImage; const AFileName: string): Boolean; virtual;
    procedure SaveCssProperties(const AFileName, AStyleText: string);
    function EnsureDirectoryExists(const APath: string): Boolean; virtual;
    function CreateNextFileName(const APath, AName, AExtension: string): string; virtual;
    function GetInitialFileIndex(const APath, AName, AExtension: string): Integer; virtual;
    function GetImageFileExtension(AImage: TdxOfficeImage): string; virtual;
  public
    function CreateImageUri(const ARootUri: string; AImage: TdxOfficeImage; const ARelativeUri: string): string;
    function CreateCssUri(const ARootUri, AStyleText, ARelativeUri: string): string;
  end;

  { TdxDataStringUriProvider }

  TdxDataStringUriProvider = class(TInterfacedObject, IdxUriProvider)
  strict private type
  {$REGION 'private type'}
    TImageBytes = record
      FBytes: TArray<Byte>;
      FFormat: TdxOfficeImageFormat;
    public
      constructor Create(const ABytes: TArray<Byte>; AFormat: TdxOfficeImageFormat);
      property Bytes: TArray<Byte> read FBytes;
      property Format: TdxOfficeImageFormat read FFormat;
    end;
  {$ENDREGION}
  strict private
    function GetImageBytes(AImage: TdxOfficeImage): TImageBytes;
    function GetImageBytesCore(AImage: TdxOfficeImage; AImageFormat: TdxOfficeImageFormat): TImageBytes;
  public
    function CreateImageUri(const ARootUri: string; AImage: TdxOfficeImage; const ARelativeUri: string): string;
    function CreateCssUri(const ARootUri, AStyleText, ARelativeUri: string): string;
  end;

implementation

uses
  Windows, Forms, IOUtils, WinInet, RegularExpressions,
  dxGDIPlusClasses, dxBase64,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper,
  dxUriRecord, dxWinInet;

type

  { TDataStringUriPattern }

  TDataStringUriPattern = class sealed
  strict private const
  {$REGION 'private const'}
    CapacityPattern = '(?<capacity>\w+)';
    ContentPattern = '(?<content>.*)';
    ImagePattern = '(?<image>.*)';
    ImageTypePattern = '(?<imagetype>\w*)';
    MediaTypePattern = '(?<mediatype>[-\+\w]+)';
    SlashPattern = '\/';
    SpacePattern = '\s*';
  {$ENDREGION}
  strict private
    class function GetApplicationDataStringPattern: string; static;
    class function GetImageDataStringPattern: string; static;
  public
    class property ApplicationDataStringPattern: string read GetApplicationDataStringPattern;
    class property ImageDataStringPattern: string read GetImageDataStringPattern;
  end;

{ TDataStringUriPattern }

class function TDataStringUriPattern.GetApplicationDataStringPattern: string;
begin
  Result := Format('%0:s?data:%0:s?application%0:s?%4:s?%0:s?%1:s?%0:s?;%0:s?%2:s%0:s?,%0:s?%3:s%0:s?', [SpacePattern, MediaTypePattern, CapacityPattern, ContentPattern, SlashPattern]);
end;

class function TDataStringUriPattern.GetImageDataStringPattern: string;
begin
  Result := Format('%0:s?data:%0:s?image%0:s?%4:s?%0:s?%1:s?%0:s?;%0:s?%2:s%0:s?,%0:s?%3:s%0:s?', [SpacePattern, ImageTypePattern, CapacityPattern, ImagePattern, SlashPattern]);
end;

{ TdxUriStreamService }

constructor TdxUriStreamService.Create;
begin
  inherited Create;
  FProviders := TdxUriStreamProviderCollection.Create;
  RegisterDefaultProviders;
end;

destructor TdxUriStreamService.Destroy;
begin
  FreeAndNil(FProviders);
  inherited Destroy;
end;

function TdxUriStreamService.GetStream(const AUri: string): TStream;
begin
  Result := AsyncGetStream(AUri, nil);
end;

function TdxUriStreamService.AsyncGetStream(const AUri: string; const ACancelStatus: TdxTaskCancelCallback): TStream;
var
  ACount, I: Integer;
  AProvider2: IdxUriStreamProvider2;
begin
  if Trim(AUri) = '' then
    Exit(nil);

  Result := nil;
  ACount := Providers.Count;
  for I := 0 to ACount - 1 do
  begin
    try
      if Assigned(ACancelStatus) and ACancelStatus then
        Exit;

      if Supports(Providers[I], IdxUriStreamProvider2, AProvider2) then
        Result := AProvider2.AsyncGetStream(AUri, ACancelStatus)
      else
        Result := Providers[I].GetStream(AUri);
      if Result <> nil then
        Break;
    except
      Result := nil;
    end;
  end;
end;

procedure TdxUriStreamService.RegisterProvider(const AProvider: IdxUriStreamProvider);
begin
  if AProvider = nil then
    Exit;

  Providers.Insert(0, AProvider);
end;

procedure TdxUriStreamService.UnregisterProvider(const AProvider: IdxUriStreamProvider);
var
  AIndex: Integer;
begin
  if AProvider = nil then
    Exit;

  AIndex := Providers.IndexOf(AProvider);
  if AIndex >= 0 then
    Providers.Delete(AIndex);
end;

function TdxUriStreamService.IsAsyncLoadingSupported(const AUri: string): Boolean;
var
  ACount, I: Integer;
begin
  Result := True;
  ACount := Providers.Count;
  for I := 0 to ACount - 1 do
  begin
    if not Providers[I].IsAsyncLoadingSupported(AUri) then
      Exit(False);
  end;
end;

procedure TdxUriStreamService.RegisterDefaultProviders;
begin
  RegisterProvider(TdxWebUriStreamProvider.Create);
  RegisterProvider(TdxLocalUriStreamProvider.Create);
end;

{ TdxLocalUriStreamProvider }

function TdxLocalUriStreamProvider.GetStream(const AUri: string): TStream;
var
  AUriRecord: TdxUri;
begin
  Result := nil;
  AUriRecord := TdxUri.Create(AUri);
  if AUriRecord.IsFileScheme and TFile.Exists(AUriRecord.LocalPath) then
    try
      Result := TdxMemoryStream.Create(AUriRecord.LocalPath);
      Result.Position := 0;
    except
      FreeAndNil(Result);
    end;
end;

function TdxLocalUriStreamProvider.IsAsyncLoadingSupported(const AUrl: string): Boolean;
begin
  Result := True;
end;

{ TdxWebUriStreamProvider }

function TdxWebUriStreamProvider.GetStream(const AUri: string): TStream;
begin
  Result := AsyncGetStream(AUri, nil);
end;

function TdxWebUriStreamProvider.AsyncGetStream(const AUri: string; const ACancelStatus: TdxTaskCancelCallback): TStream;

  function IsCancel: Boolean;
  begin
    Result := Assigned(ACancelStatus) and ACancelStatus;
  end;

var
  AUriRecord: TdxUri;
  AFile: string;
begin
  Result := nil;
  AUriRecord := TdxUri.Create(AUri);
  if AUriRecord.IsWebScheme then
  begin
    try
      if IsCancel then
        Exit;
      Result := TMemoryStream.Create;
      AFile := AUriRecord.URI;
      if TdxHttpHelper.GetStream(Application.Title, AFile, '', Result) and
          not IsCancel and TdxImageLoaderHelper.IsImageStream(Result) then
        Result.Position := 0
      else
        FreeAndNil(Result);
    except
      FreeAndNil(Result);
    end;
  end;
end;

function TdxWebUriStreamProvider.IsAsyncLoadingSupported(const AUrl: string): Boolean;
begin
  Result := True;
end;

{ TdxDataStringUriStreamProvider }

function TdxDataStringUriStreamProvider.GetStream(const AUri: string): TStream;

  function ConvertToStream(const Value: string): TStream;
  var
    ABuffer: TArray<Byte>;
  begin
    try
      ABuffer := TdxBase64.FromBase64String(Value);
    except
      ABuffer := TdxBase64.FromBase64String(TdxUri.UnescapeDataString(Value));
    end;
    Result := TBytesStream.Create(ABuffer);
  end;

var
  AMatch: TMatch;
  ARegex: TRegEx;
begin
  Result := nil;
  if not TdxStringHelper.StartsWith(AUri, 'data:') then
    Exit;

  try
    ARegex := TRegEx.Create(TDataStringUriPattern.ImageDataStringPattern, [{$IFDEF DELPHIXE6}roNotEmpty, {$ENDIF}roSingleLine]);
    AMatch := ARegex.Match(AUri);
    if AMatch.Success then
    begin
      if AMatch.Groups[2{'capacity'}].Value = 'base64' then
        Result := ConvertToStream(AMatch.Groups['image'].Value);
    end
    else
    begin
      ARegex := TRegEx.Create(TDataStringUriPattern.ApplicationDataStringPattern);
      AMatch := ARegex.Match(AUri);
      if AMatch.Success then
      begin
        if (AMatch.Groups[1{'mediatype'}].Value = 'octet-stream') and (AMatch.Groups['capacity'].Value = 'base64') then
          Result := ConvertToStream(AMatch.Groups['content'].Value);
      end;
    end;
  except
    FreeAndNil(Result);
  end;
  if Result <> nil then
    Result.Position := 0;
end;

function TdxDataStringUriStreamProvider.IsAsyncLoadingSupported(const AUrl: string): Boolean;
begin
  Result := True;
end;

{ TdxUriProviderService }

constructor TdxUriProviderService.Create;
begin
  inherited Create;
  FProviders := TdxUriProviderCollection.Create;
  RegisterDefaultProviders;
end;

destructor TdxUriProviderService.Destroy;
begin
  FreeAndNil(FProviders);
  inherited Destroy;
end;

procedure TdxUriProviderService.RegisterDefaultProviders;
begin
  RegisterProvider(TdxFileBasedUriProvider.Create);
end;

function TdxUriProviderService.CreateImageUri(const ARootUri: string;
  AImage: TdxOfficeImage; const ARelativeUri: string): string;
var
  I, ACount: Integer;
begin
  Result := '';
  ACount := Providers.Count;
  for I := 0 to ACount - 1 do
  begin
    Result := Providers[I].CreateImageUri(ARootUri, AImage, ARelativeUri);
    if Result <> '' then
      Exit;
  end;
end;

function TdxUriProviderService.CreateCssUri(const ARootUri, AStyleText, ARelativeUri: string): string;
var
  I, ACount: Integer;
begin
  Result := '';
  ACount := Providers.Count;
  for I := 0 to ACount - 1 do
  begin
    Result := Providers[I].CreateCssUri(ARootUri, AStyleText, ARelativeUri);
    if Result <> '' then
      Exit;
  end;
end;

procedure TdxUriProviderService.RegisterProvider(const AProvider: IdxUriProvider);
begin
  if AProvider = nil then
    Exit;

  Providers.Insert(0, AProvider);
end;

procedure TdxUriProviderService.UnregisterProvider(const AProvider: IdxUriProvider);
var
  AIndex: Integer;
begin
  if AProvider = nil then
    Exit;

  AIndex := Providers.IndexOf(AProvider);
  if AIndex >= 0 then
    Providers.Delete(AIndex);
end;

{ TdxFileBasedUriProvider }

function TdxFileBasedUriProvider.CreateImageUri(const ARootUri: string; AImage: TdxOfficeImage; const ARelativeUri: string): string;
var
  AFileName: string;
  S: string;
begin
  if AImage = nil then
    Exit('');

  S := TPath.GetDirectoryName(ARootUri);
  if not EnsureDirectoryExists(S) then
    Exit('');

  AFileName := CreateNextFileName(S, 'image', GetImageFileExtension(AImage));
  if TryToSaveImageInNativeFormat(AImage, AFileName) then
  begin
    if ARelativeUri <> '' then
      Result := GetRelativeFileName(S, ARelativeUri, AFileName)
    else
      Result := FileUriPrefix + AFileName;
    Exit;
  end;

  AFileName := CreateNextFileName(S, 'image', 'png');
  if TryToSaveImageAsPng(AImage, AFileName) then
  begin
    if ARelativeUri > '' then
      Result := GetRelativeFileName(S, ARelativeUri, AFileName)
    else
      Result := FileUriPrefix + AFileName;
  end
  else
    Result := '';
end;

function TdxFileBasedUriProvider.CreateCssUri(const ARootUri, AStyleText, ARelativeUri: string): string;
var
  AFileName: string;
  S: string;
begin
  if AStyleText = '' then
    Exit('');
  S := TPath.GetDirectoryName(ARootUri);
  if not EnsureDirectoryExists(S) then
    Exit('');
  AFileName := CreateNextFileName(S, 'style', 'css');
  SaveCssProperties(AFileName, AStyleText);
  if ARelativeUri <> '' then
    Result := GetRelativeFileName(S, ARelativeUri, AFileName)
  else
    Result := FileUriPrefix + AFileName;
end;

function TdxFileBasedUriProvider.GetRelativeFileName(const ARootUri, ARelativeUri, AFileName: string): string;
var
  AStr: TStringBuilder;
begin
  AStr := TStringBuilder.Create(AFileName);
  try
    try
      if Length(ARootUri) > 0 then
        AStr.Remove(0, Length(ARootUri) + 1);
      AStr.Insert(0, ARelativeUri);
    except
      Result := FileUriPrefix + AFileName;
      TdxRichEditExceptions.ThrowArgumentException('invalid relativeUri', ARelativeUri);
    end;
    Result := AStr.ToString;
  finally
    AStr.Free;
  end;
end;

function TdxFileBasedUriProvider.TryToSaveImageInNativeFormat(AImage: TdxOfficeImage; const AFileName: string): Boolean;
begin
  if AImage.RawFormat = TdxOfficeImageFormat.MemoryBmp then
    Result := TryToSaveImageAsPng(AImage, AFileName)
  else
    try
      AImage.SaveToFile(AFileName);
      Result := True;
    except
      Result := False;
    end;
end;

function TdxFileBasedUriProvider.TryToSaveImageAsPng(AImage: TdxOfficeImage; const AFileName: string): Boolean;
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    try
      AImage.SaveToStreamByCodec(AStream, dxImagePng);
      AStream.Position := 0;
      AStream.SaveToFile(AFileName);
      Result := True;
    except
      Result := False;
    end;
  finally
    AStream.Free;
  end;
end;

procedure TdxFileBasedUriProvider.SaveCssProperties(const AFileName, AStyleText: string);
var
  AStream: TFileStream;
  AWriter: TStreamWriter;
begin
  AStream := TFileStream.Create(AFileName, fmCreate or fmOpenReadWrite);
  try
    AWriter := TStreamWriter.Create(AStream, TEncoding.ASCII);
    try
      AWriter.Write(AStyleText);
    finally
      AWriter.Free;
    end;
  finally
    AStream.Free;
  end;
end;

function TdxFileBasedUriProvider.EnsureDirectoryExists(const APath: string): Boolean;
begin
  if not TDirectory.Exists(APath) then
    TDirectory.CreateDirectory(APath);

  Result := TDirectory.Exists(APath);
end;

function TdxFileBasedUriProvider.CreateNextFileName(const APath, AName, AExtension: string): string;
var
  I: Integer;
  AFileName: string;
begin
  I := GetInitialFileIndex(APath, AName, AExtension);
  while True do
  begin
    AFileName := Format('%s%d.%s', [AName, I, AExtension]);
    AFileName := TPath.Combine(APath, AFileName);
    if not TFile.Exists(AFileName) then
    begin
      FLastFileIndex := I;
      Exit(AFileName);
    end;
    Inc(I);
  end;
end;

function TdxFileBasedUriProvider.GetInitialFileIndex(const APath, AName, AExtension: string): Integer;
var
  S: string;
begin
  S := TPath.GetFullPath(APath);
  if (CompareText(S, FLastPath) = 0) and
    (CompareText(AName, FLastName) = 0) and
    (CompareText(AExtension, FLastExtension) = 0) then
  begin
    Result := FLastFileIndex + 1;
  end
  else
  begin
    FLastPath := S;
    FLastName := AName;
    FLastExtension := AExtension;
    FLastFileIndex := 0;
    Result := FLastFileIndex;
  end;
end;

function TdxFileBasedUriProvider.GetImageFileExtension(AImage: TdxOfficeImage): string;
begin
  Result := TdxOfficeImage.GetExtension(AImage.RawFormat);
  if Result <> '' then
    Exit;
  if AImage.RawFormat = TdxOfficeImageFormat.MemoryBmp then
    Result := 'png'
  else
    Result := 'img';
end;


{ TdxDataStringUriProvider.TImageBytes }

constructor TdxDataStringUriProvider.TImageBytes.Create(
  const ABytes: TArray<Byte>; AFormat: TdxOfficeImageFormat);
begin
  FBytes := ABytes;
  FFormat := AFormat;
end;

{ TdxDataStringUriProvider }

function TdxDataStringUriProvider.CreateCssUri(const ARootUri, AStyleText,
  ARelativeUri: string): string;
begin
  Result := '';
end;

function TdxDataStringUriProvider.CreateImageUri(const ARootUri: string;
  AImage: TdxOfficeImage; const ARelativeUri: string): string;
var
  ABytes: TImageBytes;
  AContentType: string;
begin
  if AImage = nil then
    Exit('');
  try
    ABytes := GetImageBytes(AImage);
    AContentType := TdxOfficeImage.GetContentType(ABytes.Format);
    Result := Format('data:%s;base64,%s', [AContentType, TdxBase64.ToBase64String(ABytes.Bytes)]);
  except
    Result := '';
  end;
end;

function TdxDataStringUriProvider.GetImageBytes(
  AImage: TdxOfficeImage): TImageBytes;
begin
  try
    Result := GetImageBytesCore(AImage, AImage.RawFormat);
  except
    Result := GetImageBytesCore(AImage, TdxOfficeImageFormat.Png);
  end;
end;

function TdxDataStringUriProvider.GetImageBytesCore(AImage: TdxOfficeImage;
  AImageFormat: TdxOfficeImageFormat): TImageBytes;
var
  ABytes: TArray<Byte>;
begin
  ABytes := AImage.GetImageBytesSafe(AImageFormat);
  Result := TImageBytes.Create(ABytes, AImageFormat);
end;

end.
