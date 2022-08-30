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

unit dxRichEdit.DocumentModel.Hyperlink;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, Classes, Contnrs, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core;

type
  TdxHyperlinkInstructionBuilder = class;

  { TdxTextToDisplaySource }

  TdxTextToDisplaySource  = (
    ExistingText,
    NewText
  );

  { TdxHyperlinkInfo }

  TdxHyperlinkInfo = class(TdxCloneable)
  strict private
    class var
      FEmpty: TdxHyperlinkInfo;
  strict private
    FAnchor: string;
    FNavigateUri: string;
    FTarget: string;
    FToolTip: string;
    FVisited: Boolean;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function GetAnchor: string;
    function GetNavigateUri: string;
    function GetTarget: string;
    function GetToolTip: string;
    function GetVisited: Boolean;
    procedure SetAnchor(const Value: string);
    procedure SetNavigateUri(const Value: string);
    procedure SetTarget(const Value: string);
    procedure SetToolTip(const Value: string);
    procedure SetVisited(const Value: Boolean);

    function GetActualToolTip: string;
  public
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxHyperlinkInfo; reintroduce; inline;
    function CreateUrl: string;

    property Anchor: string read FAnchor write FAnchor;
    property NavigateUri: string read FNavigateUri write FNavigateUri;
    property Target: string read FTarget write FTarget;
    property ToolTip: string read FToolTip write FToolTip;
    property Visited: Boolean read FVisited write FVisited;

    class property Empty: TdxHyperlinkInfo read FEmpty;
  end;

  { TdxHyperlinkInfoCollection }

  TdxHyperlinkInfoCollection = class(TEnumerable<Integer>)
  strict private
    FHyperlinkInfos: TdxObjectList<TdxHyperlinkInfo>;
    FHyperlinkIndexes: TdxIntegersDictionary;
    function GetCount: Integer;
  private
    function GetItem(AFieldIndex: Integer): TdxHyperlinkInfo;
    procedure SetItem(AFieldIndex: Integer; const Value: TdxHyperlinkInfo);
  protected
    function DoGetEnumerator: TEnumerator<Integer>; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AFieldIndex: Integer; AInfo: TdxHyperlinkInfo);
    function IsHyperlink(AFieldIndex: Integer): Boolean;
    function TryGetHyperlinkInfo(AFieldIndex: Integer; var AInfo: TdxHyperlinkInfo): Boolean;
    // for internal use
    procedure RecalcKeys(AFrom: Integer; ADeltaIndex: Integer); virtual;

    property Count: Integer read GetCount;
    property Items[AFieldIndex: Integer]: TdxHyperlinkInfo read GetItem write SetItem; default;
  end;

  { TdxHyperlinkInstructionBuilder }

  TdxHyperlinkInstructionBuilder = class
  strict protected type
    TGetSwitchValue = reference to function(ABuilder: TdxHyperlinkInstructionBuilder): string;
    TAttributes = class(TdxNamedDelegateDictionary<TGetSwitchValue>);
  strict private
    FAttributes: TAttributes;
    FHyperlinkInfo: TdxHyperlinkInfo;
    class function CreateAttributes: TAttributes; static;
    class function GetAnchor(ABuilder: TdxHyperlinkInstructionBuilder): string; static;
    class function GetTarget(ABuilder: TdxHyperlinkInstructionBuilder): string; static;
    class function GetTooltip(ABuilder: TdxHyperlinkInstructionBuilder): string; static;
  protected
    function GetAttributes: TAttributes; virtual;
    function GetFieldType: string; virtual;
    function GetIndividualSwitches: string; virtual;
    function GetArgument: string; virtual;
  public
    constructor Create(AInfo: TdxHyperlinkInfo);
    destructor Destroy; override;
    function GetFieldInstruction: string; virtual;

    property HyperlinkInfo: TdxHyperlinkInfo read FHyperlinkInfo;
    property Attributes: TAttributes read GetAttributes;
    property FieldType: string read GetFieldType;
  end;

  { TdxHyperlinkUriHelper }

  TdxHyperlinkUriHelper = class
  strict private const
    FileNamePattern = '^file:[\\/]*(?<path>(?<root>[A-Za-z]:[\\/]+)?[^:\*\?<>\|]+)$';
    UrlPattern = '^((?<prefix>http|https|ftp|news)\:[\\/]*)(?<url>[a-zA-Z0-9\-\.]+(\.[a-zA-Z]{2,3})?(\:\d{1,5})?([\\/]\S*)?)$';
    RelativePathPattern = '^(?:\.{1,2}[\\/])*[^:\*\?<>\|\.\\/][^:\*\?<>\|]*$';
    LocalPathPattern = '^[A-Za-z]:[\\/]+[^:\*\?<>\|]*$';
    RemotePathPattern = '^\\{2,}(?<path>[^:\*\?<>\|]+)$';
    MailAddressPattern = '^mailto:(?<email>\S+)$';
    SlashPattern = '[\\/]+';
  public
    class function EscapeHyperlinkFieldParameterString(const AValue: string): string; static;
    class function ConvertToHyperlinkUri(const AUri: string): string; static;
    class function PrepareHyperlinkTooltipQuotes(const AValue: string): string; static;
    class function ConvertFromHyperlinkUri(const AUri: string): string; static;
    class function ConvertToUrl(const AUri: string): string; static;
    class function GetRemotePath(const AUri: string): string; static;
    class function EnsureUriIsValid(const AUri: string): string; static;
    class function EnsureFileUriIsValid(const AUri: string): string; static;
    class function EnsureUrlIsValid(const AUri: string): string; static;
    class function EnsureLocalPathIsValid(const AUri: string): string; static;
    class function EnsureRemotePathIsValid(const AUri: string): string; static;
    class function EnsureMailAddressIsValid(const AUri: string): string; static;
    class function EnsureRelativePathIsValid(const AUri: string): string; static;
    class function ConvertRelativePathToAbsolute(const AUri: string; const ABaseUri: string): string; static;
    class function IsFileUri(const AUri: string): Boolean; static;
    class function IsUrl(const AUri: string): Boolean; static;
    class function IsMailAddress(const AUri: string): Boolean; static;
    class function IsLocalPath(const AUri: string): Boolean; static;
    class function IsRemotePath(const AUri: string): Boolean; static;
    class function IsRelativePath(const AUri: string): Boolean; static;
    class function IsMatch(const AUri: string; const APattern: string): Boolean; static;
    class function Replace(const AUri: string; const APattern: string; const AReplacement: string): string; static;
    class function Split(const AUri: string; const APattern: string): TArray<string>; static;
  end;

  { TdxHyperlinkCalculator }

  TdxHyperlinkCalculator = class
  strict private type
    TdxRecord = record
      Start: TdxRunIndex;
      &End: TdxRunIndex;
      Url: string;
      Anchor: string;
      constructor Create(AStart, AEnd: TdxRunIndex; const AUrl, AAnchor: string);
    end;
  strict private
    FUrls: TList<TdxRecord>;
    FCount: Integer;
    FEmpty: Boolean;
    FCurrent: Integer;
  public
    constructor Create(PieceTable: TdxCustomPieceTable{TdxPieceTable});
    destructor Destroy; override;
    function GetAnchor(ARunIndex: TdxRunIndex): string;
    function GetUrl(ARunIndex: TdxRunIndex): string;
  end;

implementation

uses
  RegularExpressions,
  dxCore,
  dxRichEdit.Strs,
  dxStringHelper,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Fields.Core;

{ TdxHyperlinkInfo }

class constructor TdxHyperlinkInfo.Initialize;
begin
  FEmpty := TdxHyperlinkInfo.Create;
end;

class destructor TdxHyperlinkInfo.Finalize;
begin
  FreeAndNil(FEmpty);
end;

function TdxHyperlinkInfo.GetActualToolTip: string;
begin
  if ToolTip <> '' then
    Exit(ToolTip);
  if (NavigateUri = '') and ((Length(Anchor) > 0) and (Anchor[1] = '_')) then
    Exit(cxGetResourceString(@sdxCurrentDocumentHyperlinkTooltip));
  Result := Format('%s#%s', [NavigateUri, Anchor]);
  Result := TdxStringHelper.Trim(Result, ['#']);
end;

function TdxHyperlinkInfo.GetAnchor: string;
begin
  Result := FAnchor;
end;

function TdxHyperlinkInfo.GetNavigateUri: string;
begin
  Result := FNavigateUri;
end;

function TdxHyperlinkInfo.GetTarget: string;
begin
  Result := FTarget;
end;

function TdxHyperlinkInfo.GetToolTip: string;
begin
  Result := FToolTip;
end;

function TdxHyperlinkInfo.GetVisited: Boolean;
begin
  Result := FVisited;
end;

procedure TdxHyperlinkInfo.SetAnchor(const Value: string);
begin
  FAnchor := Value;
end;

procedure TdxHyperlinkInfo.SetNavigateUri(const Value: string);
begin
  FNavigateUri := Value;
end;

procedure TdxHyperlinkInfo.SetTarget(const Value: string);
begin
  FTarget := Value;
end;

procedure TdxHyperlinkInfo.SetToolTip(const Value: string);
begin
  FToolTip := Value;
end;

procedure TdxHyperlinkInfo.SetVisited(const Value: Boolean);
begin
  FVisited := Value;
end;

function TdxHyperlinkInfo.CreateUrl: string;
begin
  if NavigateUri = '' then
    Result := ''
  else
    if Anchor <> '' then
      Result := Format('%s#%s', [NavigateUri, Anchor])
    else
      Result := NavigateUri;
end;

function TdxHyperlinkInfo.Clone: TdxHyperlinkInfo;
begin
  Result := TdxHyperlinkInfo(inherited Clone);
end;

procedure TdxHyperlinkInfo.CopyFrom(Source: TdxCloneable);
var
  AValue: TdxHyperlinkInfo absolute Source;
begin
  NavigateUri := AValue.NavigateUri;
  Anchor := AValue.Anchor;
  ToolTip := AValue.ToolTip;
  Target := AValue.Target;
  Visited := AValue.Visited;
end;

{ TdxHyperlinkInfoCollection }

constructor TdxHyperlinkInfoCollection.Create;
begin
  inherited Create;
  FHyperlinkInfos := TdxObjectList<TdxHyperlinkInfo>.Create;
  FHyperlinkIndexes := TdxIntegersDictionary.Create;
end;

destructor TdxHyperlinkInfoCollection.Destroy;
begin
  FreeAndNil(FHyperlinkIndexes);
  FreeAndNil(FHyperlinkInfos);
  inherited Destroy;
end;

function TdxHyperlinkInfoCollection.GetCount: Integer;
begin
  Result := FHyperlinkInfos.Count;
end;

function TdxHyperlinkInfoCollection.GetItem(
  AFieldIndex: Integer): TdxHyperlinkInfo;
var
  AIndex: Integer;
begin
  AIndex := FHyperlinkIndexes[AFieldIndex];
  Result := FHyperlinkInfos[AIndex];
end;

procedure TdxHyperlinkInfoCollection.Clear;
begin
  FHyperlinkInfos.Clear;
  FHyperlinkIndexes.Clear;
end;

procedure TdxHyperlinkInfoCollection.Add(AFieldIndex: Integer; AInfo: TdxHyperlinkInfo);
var
  AIndex: Integer;
begin
  AIndex := FHyperlinkInfos.IndexOf(AInfo);
  if AIndex = -1 then
    AIndex := FHyperlinkInfos.Add(AInfo);
  FHyperlinkIndexes.Add(AFieldIndex, AIndex);
end;

procedure TdxHyperlinkInfoCollection.SetItem(AFieldIndex: Integer;
  const Value: TdxHyperlinkInfo);
var
  AIndex: Integer;
begin
  AIndex := FHyperlinkInfos.IndexOf(Value);
  if AIndex = -1 then
    AIndex := FHyperlinkInfos.Add(Value);
  FHyperlinkIndexes.AddOrSetValue(AFieldIndex, AIndex);
end;

function TdxHyperlinkInfoCollection.IsHyperlink(AFieldIndex: Integer): Boolean;
begin
  Result := FHyperlinkIndexes.ContainsKey(AFieldIndex);
end;

function TdxHyperlinkInfoCollection.TryGetHyperlinkInfo(AFieldIndex: Integer; var AInfo: TdxHyperlinkInfo): Boolean;
var
  AIndex: Integer;
begin
  Result := FHyperlinkIndexes.TryGetValue(AFieldIndex, AIndex);
  if Result then
    AInfo := FHyperlinkInfos[AIndex];
end;

procedure TdxHyperlinkInfoCollection.RecalcKeys(AFrom: Integer; ADeltaIndex: Integer);
var
  AResult: TdxIntegersDictionary;
  ARow: TPair<Integer, Integer>;
begin
  AResult := TdxIntegersDictionary.Create;
  for ARow in FHyperlinkIndexes do
  begin
    if ARow.Key >= AFrom then
      AResult.AddOrSetValue(ARow.Key + ADeltaIndex, ARow.Value)
    else
      AResult.AddOrSetValue(ARow.Key, ARow.Value);
  end;
  FreeAndNil(FHyperlinkIndexes);
  FHyperlinkIndexes:= AResult;
end;

function TdxHyperlinkInfoCollection.DoGetEnumerator: TEnumerator<Integer>;
begin
  Result := FHyperlinkIndexes.Keys.GetEnumerator;
end;

{ TdxHyperlinkInstructionBuilder }

constructor TdxHyperlinkInstructionBuilder.Create(AInfo: TdxHyperlinkInfo);
begin
  inherited Create;
  FAttributes := CreateAttributes;
  FHyperlinkInfo := AInfo;
end;

destructor TdxHyperlinkInstructionBuilder.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

class function TdxHyperlinkInstructionBuilder.CreateAttributes: TAttributes;
begin
  Result := TAttributes.Create;
  Result.Add('l', GetAnchor);
  Result.Add('o', GetTooltip);
  Result.Add('t', GetTarget);
end;

class function TdxHyperlinkInstructionBuilder.GetAnchor(ABuilder: TdxHyperlinkInstructionBuilder): string;
begin
  Result := ABuilder.HyperlinkInfo.Anchor;
end;

class function TdxHyperlinkInstructionBuilder.GetTarget(ABuilder: TdxHyperlinkInstructionBuilder): string;
begin
  Result := ABuilder.HyperlinkInfo.Target;
end;

class function TdxHyperlinkInstructionBuilder.GetTooltip(ABuilder: TdxHyperlinkInstructionBuilder): string;
begin
  Result := ABuilder.HyperlinkInfo.ToolTip;
  if Result = '' then
    Exit;
  Result := TdxHyperlinkUriHelper.EscapeHyperlinkFieldParameterString(Result);
  Result := TdxHyperlinkUriHelper.PrepareHyperlinkTooltipQuotes(Result);
end;

function TdxHyperlinkInstructionBuilder.GetAttributes: TAttributes;
begin
  Result := FAttributes;
end;

function TdxHyperlinkInstructionBuilder.GetFieldType: string;
begin
  Result := 'HYPERLINK';
end;

function TdxHyperlinkInstructionBuilder.GetFieldInstruction: string;
var
  ABuilder: TStringBuilder;
  AArgument, AIndividualSwitches: string;
begin
  ABuilder := TStringBuilder.Create(FieldType);
  try
    AArgument := GetArgument;
    if AArgument <> '' then
      ABuilder.AppendFormat(' "%s"', [AArgument]);
    AIndividualSwitches := GetIndividualSwitches;
    if AIndividualSwitches <> '' then
    begin
      ABuilder.Append(' ');
      ABuilder.Append(AIndividualSwitches);
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxHyperlinkInstructionBuilder.GetIndividualSwitches: string;
var
  ABuilder: TStringBuilder;
  ACallback: TGetSwitchValue;
  AValue: string;
  ANames: TArray<string>;
  AName: string;
  AAdded: Boolean;
begin
  ABuilder := TStringBuilder.Create;
  try
    AAdded := False;
    ANames := Attributes.Keys;
    for AName in ANames do
    begin
      Attributes.TryGetValue(AName, ACallback);
      AValue := ACallback(Self);
      if AValue <> '' then
      begin
        if AAdded then
          ABuilder.Append(' ');
        ABuilder.Append(Format('\%s "%s"', [AName, AValue]));
        AAdded := True;
      end;
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxHyperlinkInstructionBuilder.GetArgument: string;
begin
  if HyperlinkInfo.NavigateUri = '' then
    Exit('');

  Result := TdxHyperlinkUriHelper.ConvertToHyperlinkUri(HyperlinkInfo.NavigateUri);
end;

{ TdxHyperlinkUriHelper }

class function TdxHyperlinkUriHelper.EscapeHyperlinkFieldParameterString(const AValue: string): string;
begin
  Result := Replace(AValue, '\\', '\\');
end;

class function TdxHyperlinkUriHelper.ConvertToHyperlinkUri(const AUri: string): string;
begin
  Result := Replace(AUri, '\\', '\\\\');
end;

class function TdxHyperlinkUriHelper.PrepareHyperlinkTooltipQuotes(const AValue: string): string;
begin
  Result := Replace(AValue, '\"', '\\\"');
end;

class function TdxHyperlinkUriHelper.ConvertFromHyperlinkUri(const AUri: string): string;
begin
  Result := Replace(AUri, '\\\\', '\\');
end;

class function TdxHyperlinkUriHelper.ConvertToUrl(const AUri: string): string;
begin
  if IsLocalPath(AUri) then
    Exit(TRegEx.Replace(Format('file:///%s', [AUri]), '\\+', '/'));
  if IsRemotePath(AUri) then
    Exit(TRegEx.Replace(Format('file://%s', [GetRemotePath(AUri)]), '\\+', '/'));
  Result := AUri;
end;

class function TdxHyperlinkUriHelper.GetRemotePath(const AUri: string): string;
var
  ARegex: TRegEx;
begin
  ARegex := TRegEx.Create(RemotePathPattern);
  Result := ARegex.Match(AUri).Groups['path'].Value;
end;

class function TdxHyperlinkUriHelper.EnsureUriIsValid(const AUri: string): string;
begin
  if IsFileUri(AUri) then
    Exit(EnsureFileUriIsValid(AUri));
  if IsUrl(AUri) then
    Exit(EnsureUrlIsValid(AUri));
  if IsLocalPath(AUri) then
    Exit(EnsureLocalPathIsValid(AUri));
  if IsRemotePath(AUri) then
    Exit(EnsureRemotePathIsValid(AUri));
  if IsMailAddress(AUri) then
    Exit(EnsureMailAddressIsValid(AUri));
  if IsRelativePath(AUri) then
    Exit(EnsureRelativePathIsValid(AUri));

  Result := AUri;
end;

class function TdxHyperlinkUriHelper.EnsureFileUriIsValid(const AUri: string): string;
var
  ARegex: TRegEx;
  AMatch: TMatch;
  AEntries: TArray<string>;
  APath: string;
begin
  ARegex := TRegEx.Create(FileNamePattern);
  AMatch := ARegex.Match(AUri);

  AEntries := Split(AMatch.Groups['path'].Value, '[\\/]+');
  APath := TdxStringHelper.Join('\', AEntries);
  if (AMatch.Groups.Count > 2) and AMatch.Groups['root'].Success then
    Exit(APath);
  Result := Format('\\%s', [APath]);
end;

class function TdxHyperlinkUriHelper.EnsureUrlIsValid(const AUri: string): string;
var
  ARegex: TRegEx;
  AMatch: TMatch;
  AUrl, APrefix: string;
begin
  ARegex := TRegEx.Create(UrlPattern);
  AMatch := ARegex.Match(AUri);
  AUrl := AMatch.Groups['url'].Value;

  APrefix := AMatch.Groups['prefix'].Value;
  if APrefix = '' then
    APrefix := 'http';
  Result := Format('%s://%s', [APrefix, TRegEx.Replace(AUrl, '\\+', '/')]);
end;

class function TdxHyperlinkUriHelper.EnsureLocalPathIsValid(const AUri: string): string;
var
  AEntries: TArray<string>;
begin
  AEntries := Split(AUri, '[\\/]+');
  Result := TdxStringHelper.Join('\', AEntries);
  if Length(AEntries) = 1 then
    Exit(Result + '\');
end;

class function TdxHyperlinkUriHelper.EnsureRemotePathIsValid(const AUri: string): string;
var
  ARegex: TRegEx;
  AMatch: TMatch;
  AEntries: TArray<string>;
begin
  ARegex := TRegEx.Create(RemotePathPattern);
  AMatch := ARegex.Match(AUri);
  AEntries := Split(AMatch.Groups['path'].Value, '[\\/]+');
  Result := Format('\\%s', [TdxStringHelper.Join('\', AEntries)]);
end;

class function TdxHyperlinkUriHelper.EnsureMailAddressIsValid(const AUri: string): string;
var
  ARegex: TRegEx;
  AMatch: TMatch;
begin
  ARegex := TRegEx.Create(MailAddressPattern);
  AMatch := ARegex.Match(AUri);
  Result := Format('mailto:%s', [AMatch.Groups['email'].Value]);
end;

class function TdxHyperlinkUriHelper.EnsureRelativePathIsValid(const AUri: string): string;
var
  AParts: TdxStringList;
  I: Integer;
begin
  AParts := TdxStringList.Create;
  try
    AParts.AddRange(Split(AUri, SlashPattern));
    for I := AParts.Count - 1 downto 0 do
      if AParts[I] = '.' then
        AParts.Delete(I);
    Result := TdxStringHelper.Join('\', AParts.ToArray);
  finally
    AParts.Free;
  end;
end;

class function TdxHyperlinkUriHelper.ConvertRelativePathToAbsolute(const AUri: string; const ABaseUri: string): string;
var
  AParts: TdxStringList;
  I: Integer;
begin
  AParts := TdxStringList.Create;
  try
    AParts.AddRange(Split(ABaseUri, SlashPattern));
    AParts.Delete(AParts.Count - 1);
    AParts.AddRange(Split(AUri, SlashPattern));
    I := 0;
    while I < AParts.Count do
    begin
      if AParts[I] = '.' then
        AParts.Delete(I)
      else
        if AParts[I] = '..' then
        begin
          if I = 0 then
            AParts.Delete(0)
          else
          begin
            AParts.DeleteRange(I - 1, 2);
            Dec(I);
          end;
        end
        else
          Inc(I);
      Inc(I);
    end;
    Result := EnsureLocalPathIsValid(TdxStringHelper.Join('\', AParts.ToArray));
  finally
    AParts.Free;
  end;
end;

class function TdxHyperlinkUriHelper.IsFileUri(const AUri: string): Boolean;
begin
  Result := IsMatch(AUri, FileNamePattern);
end;

class function TdxHyperlinkUriHelper.IsUrl(const AUri: string): Boolean;
begin
  Result := IsMatch(AUri, UrlPattern);
end;

class function TdxHyperlinkUriHelper.IsMailAddress(const AUri: string): Boolean;
begin
  Result := IsMatch(AUri, MailAddressPattern);
end;

class function TdxHyperlinkUriHelper.IsLocalPath(const AUri: string): Boolean;
begin
  Result := IsMatch(AUri, LocalPathPattern);
end;

class function TdxHyperlinkUriHelper.IsRemotePath(const AUri: string): Boolean;
begin
  Result := IsMatch(AUri, RemotePathPattern);
end;

class function TdxHyperlinkUriHelper.IsRelativePath(const AUri: string): Boolean;
begin
  Result := IsMatch(AUri, RelativePathPattern);
end;

class function TdxHyperlinkUriHelper.IsMatch(const AUri: string; const APattern: string): Boolean;
var
  ARegex: TRegEx;
begin
  ARegex := TRegEx.Create(APattern);
  Result := ARegex.IsMatch(AUri);
end;

class function TdxHyperlinkUriHelper.Replace(const AUri: string; const APattern: string; const AReplacement: string): string;
var
  ARegex: TRegEx;
begin
  ARegex := TRegEx.Create(APattern);
  Result := ARegex.Replace(AUri, AReplacement);
end;

class function TdxHyperlinkUriHelper.Split(const AUri: string; const APattern: string): TArray<string>;
begin
  Result := TRegEx.Split(AUri, APattern);
end;

{ TdxHyperlinkCalculator.TdxRecord }

constructor TdxHyperlinkCalculator.TdxRecord.Create(AStart, AEnd: TdxRunIndex; const AUrl, AAnchor: string);
begin
  Start := AStart;
  &End := AEnd;
  Url := AUrl;
  Anchor := AAnchor;
end;

{ TdxHyperlinkCalculator }

constructor TdxHyperlinkCalculator.Create(PieceTable: TdxCustomPieceTable{TdxPieceTable});
var
  I: Integer;
  AUrl: string;
  AField: TdxField;
  AInfo: TdxHyperlinkInfo;
  AHyperlinkInfos: TdxHyperlinkInfoCollection;
  APieceTable: TdxSimplePieceTable absolute PieceTable;
begin
  FUrls := TList<TdxRecord>.Create;
  AHyperlinkInfos := APieceTable.HyperlinkInfos;
  for I := 0 to APieceTable.Fields.Count - 1 do
  begin
    AField := APieceTable.Fields[I];
    if AHyperlinkInfos.IsHyperlink(AField.Index) then
    begin
      AInfo := AHyperlinkInfos[AField.Index];
      if (AInfo.NavigateUri <> '') or (AInfo.Anchor <> '') then
      begin
        AUrl := AInfo.CreateUrl;
        FUrls.Add(TdxRecord.Create(AField.FirstRunIndex, AField.LastRunIndex, AUrl, AInfo.Anchor));
      end;
    end;
  end;
  FCount := FUrls.Count;
  FEmpty := FCount = 0;
  FCurrent := -1;
end;

destructor TdxHyperlinkCalculator.Destroy;
begin
  FreeAndNil(FUrls);
  inherited Destroy;
end;

function TdxHyperlinkCalculator.GetAnchor(ARunIndex: TdxRunIndex): string;
begin
  if FEmpty then
    Exit('');
  if (FCurrent >= 0) and (FUrls[FCurrent].Start > ARunIndex) then
    FCurrent := -1;
  while (FCurrent < FCount - 1) and (FUrls[FCurrent + 1].Start <= ARunIndex) do
    Inc(FCurrent);
  if ((FCurrent < 0)) or ((FUrls[FCurrent].&End < ARunIndex)) then
    Exit('');
  Result := FUrls[FCurrent].Anchor;
end;

function TdxHyperlinkCalculator.GetUrl(ARunIndex: TdxRunIndex): string;
begin
  if FEmpty then
    Exit('');
  if (FCurrent >= 0) and (FUrls[FCurrent].Start > ARunIndex) then
    FCurrent := -1;
  while (FCurrent < FCount - 1) and (FUrls[FCurrent + 1].Start <= ARunIndex) do
    Inc(FCurrent);
  if ((FCurrent < 0)) or ((FUrls[FCurrent].&End < ARunIndex)) then
    Exit('');
  Result := TdxHyperlinkUriHelper.ConvertToUrl(FUrls[FCurrent].Url);
end;

end.
