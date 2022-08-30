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

unit cxStyles;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, Generics.Collections, Generics.Defaults,
  dxCoreClasses, cxClasses, cxGraphics, cxGeometry;

type
  TcxCustomStyle = class;
  TcxStyleRepository = class;
  TcxCustomStyles = class;
  TcxCustomStylesClass = class of TcxCustomStyles;

  { IcxStyleChangeListener }

  IcxStyleChangeListener = interface
  ['{E25A5395-C1E8-4311-A281-9575F79DE862}']
    procedure StyleChanged(AStyle: TcxCustomStyle);
    procedure StyleRemoved(AStyle: TcxCustomStyle);
  end;

  { TcxCustomStyle }

  TcxCustomStyle = class(TComponent)
  strict private
    FListeners: TList;
    FStyleRepository: TcxStyleRepository;

    function GetIndex: Integer;
    procedure SetStyleRepository(Value: TcxStyleRepository);
  protected
    procedure Changed; virtual;
    procedure ReadState(Reader: TReader); override;
    procedure ChangeScale(M, D: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure AddListener(AListener: IcxStyleChangeListener); virtual;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure RemoveListener(AListener: IcxStyleChangeListener); virtual;
    procedure RestoreDefaults; virtual;
    procedure SetParentComponent(AParent: TComponent); override;
    property Index: Integer read GetIndex;
    property StyleRepository: TcxStyleRepository read FStyleRepository write SetStyleRepository;
  end;

  { TcxCustomStyleSheet }

  TcxCustomStyleSheet = class(TcxComponent)
  strict private
    FBuiltIn: Boolean;
    FCaption: string;
    FStyleRepository: TcxStyleRepository;
    FStyles: TcxCustomStyles;
    FStylesList: TList;

    function GetCaptionStored: Boolean;
    function GetCaption: string;
    function GetIndex: Integer;
    procedure SetCaption(const Value: String);
    procedure SetStyleRepository(Value: TcxStyleRepository);

    procedure ReadBuiltIn(AReader: TReader);
    procedure WriteBuiltIn(AWriter: TWriter);
  protected
    procedure DefineProperties(AFiler: TFiler); override;
    procedure DoStyleChanged(AIndex: Integer);
    procedure GetFakeComponentLinks(AList: TList); override;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddStyles(AStyles: TcxCustomStyles);
    procedure Assign(Source: TPersistent); override;
    procedure CopyFrom(AStyleSheet: TcxCustomStyleSheet);
    class function GetStylesClass: TcxCustomStylesClass; virtual;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    function GetStyles: TcxCustomStyles;
    procedure RemoveStyles(AStyles: TcxCustomStyles);
    procedure SetStyles(const Value: TcxCustomStyles);
    procedure SetParentComponent(AParent: TComponent); override;
    //
    property BuiltIn: Boolean read FBuiltIn;
    property Index: Integer read GetIndex;
    property StyleRepository: TcxStyleRepository read FStyleRepository write SetStyleRepository;
  published
    property Caption: string read GetCaption write SetCaption stored GetCaptionStored;
  end;

  TcxCustomStyleClass = class of TcxCustomStyle;
  TcxCustomStyleSheetClass = class of TcxCustomStyleSheet;

  { TcxStyleRepository }

  TcxStyleRepository = class(TcxScalableComponent)
  strict private
    FItems: TList;
    FStyleSheets: TList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TcxCustomStyle;
    function GetStyleSheetCount: Integer;
    function GetStyleSheet(Index: Integer): TcxCustomStyleSheet;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ChangeScale(M, D: Integer); override;

    procedure AddItem(AItem: TcxCustomStyle);
    procedure AddStyleSheet(AStyleSheet: TcxCustomStyleSheet);
    procedure RemoveItem(AItem: TcxCustomStyle);
    procedure RemoveStyleSheet(AStyleSheet: TcxCustomStyleSheet);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearStyleSheets;
    function CreateItem(AStyleClass: TcxCustomStyleClass): TcxCustomStyle; virtual;
    function CreateItemEx(AStyleClass: TcxCustomStyleClass; AOwner: TComponent): TcxCustomStyle; virtual;
    function CreateStyleSheet(AStyleSheetClass: TcxCustomStyleSheetClass): TcxCustomStyleSheet; virtual;
    function CreateStyleSheetEx(AStyleSheetClass: TcxCustomStyleSheetClass; AOwner: TComponent): TcxCustomStyleSheet; virtual;
    function StyleIndexOf(AStyle: TcxCustomStyle): Integer;
    function StyleSheetIndexOf(AStyleSheet: TcxCustomStyleSheet): Integer;

    property Count: Integer read GetCount;
    property StyleSheetCount: Integer read GetStyleSheetCount;
    property Items[Index: Integer]: TcxCustomStyle read GetItem; default;
    property StyleSheets[Index: Integer]: TcxCustomStyleSheet read GetStyleSheet;
  published
    property Scalable default True;
  end;

  { TcxCustomStyles }

  TcxCustomStylesItem = class
  public
    Index: Integer;
    Item: TcxCustomStyle;
    constructor Create(AIndex: Integer; AItem: TcxCustomStyle);
  end;

  TcxCustomStyles = class(TcxInterfacedPersistent, IcxStyleChangeListener)
  private
    FDestroying: Boolean;
    FItems: TList;
    FStyleSheet: TcxCustomStyleSheet;
    FOwnerStyleSheet: TcxCustomStyleSheet;  // style sheet to which styles object is aggregated
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxCustomStylesItem;
    procedure SetStyleSheet(const Value: TcxCustomStyleSheet);
  protected
    // IcxStyleChangeListener
    procedure StyleChanged(Sender: TcxCustomStyle);
    procedure StyleRemoved(Sender: TcxCustomStyle);

    procedure Changed(AIndex: Integer); virtual;
    procedure Clear;
    procedure Delete(AItemIndex: Integer);
    procedure DoChanged(AIndex: Integer); virtual;
    function Find(AIndex: Integer; var AItemIndex: Integer): Boolean;
    function GetValue(Index: Integer): TcxCustomStyle;
    procedure SetValue(Index: Integer; Value: TcxCustomStyle);

    property Count: Integer read GetCount;
    property Destroying: Boolean read FDestroying;
    property Items[Index: Integer]: TcxCustomStylesItem read GetItem;
    property OwnerStyleSheet: TcxCustomStyleSheet read FOwnerStyleSheet;
    property StyleSheet: TcxCustomStyleSheet read FStyleSheet write SetStyleSheet;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsValidStyleSheet(AStyleSheet: TcxCustomStyleSheet): Boolean;
    procedure ResetStyles;
    property Values[Index: Integer]: TcxCustomStyle read GetValue write SetValue;
  end;

  { TcxStyle }

  TcxStyleValue = (svBitmap, svColor, svFont, svTextColor);
  TcxStyleValues = set of TcxStyleValue;

  TcxStyle = class(TcxCustomStyle)
  strict private
    FAssignedValues: TcxStyleValues;
    FBitmap: TBitmap;
    FColor: TColor;
    FFont: TFont;
    FTextColor: TColor;

    procedure SetAssignedValues(Value: TcxStyleValues);
    procedure SetBitmap(Value: TBitmap);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetTextColor(Value: TColor);
    procedure BitmapChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    function IsBitmapStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsTextColorStored: Boolean;
  protected
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property AssignedValues: TcxStyleValues read FAssignedValues write SetAssignedValues default [];
    property Color: TColor read FColor write SetColor stored IsColorStored;
    property Bitmap: TBitmap read FBitmap write SetBitmap stored IsBitmapStored;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property TextColor: TColor read FTextColor write SetTextColor stored IsTextColorStored;
  end;

  TcxStyleGetName = function(AStyle: TcxStyle): string of object;

  { TcxStyles }

  TcxStyles = class(TcxCustomStyles, IcxFontListener)
  strict private
    FMaxDefaultViewParamsIndex: Integer;
    FScaledFonts: TObjectDictionary<TcxCustomStyle, TFont>;

    function GetActualFont(AStyle: TcxStyle): TFont;
  protected
    BitmapInViewParams: Boolean;

    // IcxFontListener
    procedure IcxFontListener.Changed = DefaultFontChanged;
    procedure DefaultFontChanged(Sender: TObject; AFont: TFont);

    function GetValue(Index: Integer): TcxStyle;
    procedure SetValue(Index: Integer; Value: TcxStyle);
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); virtual;
    procedure InternalGetViewParams(Index: Integer; AData: TObject; AStyle: TcxStyle; out AParams: TcxViewParams);
    function IsDefaultFont(Index: Integer): Boolean; virtual;

    property MaxDefaultViewParamsIndex: Integer read FMaxDefaultViewParamsIndex;
  public
    destructor Destroy; override;
    function GetBitmap(Index: Integer): TBitmap;
    procedure GetViewParams(Index: Integer; AData: TObject; AStyle: TcxStyle; out AParams: TcxViewParams);

    property Count;
    property StyleSheet;
    property Values[Index: Integer]: TcxStyle read GetValue write SetValue;
  end;

//function GetDefaultStyleRepository: TcxStyleRepository;
function CombineParamsWithStyle(AStyle: TcxStyle; AHasValues: TcxStyleValues; var AParams: TcxViewParams): TcxStyleValues;
function UseStyle(AStyle: TcxStyle; AStyleValue: TcxStyleValue): Boolean;
procedure CreateStyleSheetStyles(ADestStyleSheet, ASourceStyleSheet: TcxCustomStyleSheet; AStyleGetName: TcxStyleGetName = nil);

procedure RegisterStyleSheetClass(AStyleSheetClass: TcxCustomStyleSheetClass);
procedure UnregisterStyleSheetClass(AStyleSheetClass: TcxCustomStyleSheetClass);
procedure GetRegisteredStyleSheetClasses(AList: TList);

procedure SaveStyleSheetsToIniFile(const AIniFileName: string; const AList: TList);
procedure LoadStyleSheetsFromIniFile(const AIniFileName: string;
  AStyleRepository: TcxStyleRepository; AStyleSheetClass: TcxCustomStyleSheetClass;
  const AStyleSheetNames: TStrings = nil; AOwner: TComponent = nil;
  const AStyleSheetList: TList = nil; AStyleGetName: TcxStyleGetName = nil);

implementation

uses
  Controls, TypInfo, IniFiles, dxCore, dxDPIAwareUtils;

const
  BitmapCaption = 'Bitmap';
  DefaultValue = 'default';

var
  FStyleSheetClasses: TList = nil;

function BinaryStreamToString(AStream: TMemoryStream): string;
begin
  SetLength(Result, AStream.Size * 2);
  BinToHex(AStream.Memory, PChar(Result), AStream.Size);
end;

procedure StringToBinaryStream(const S: string; AStream: TMemoryStream);
begin
  AStream.Position := 0;
  AStream.Size := Length(S) div 2;
  HexToBin(PChar(S), AStream.Memory, AStream.Size);
end;

function UseStyle(AStyle: TcxStyle; AStyleValue: TcxStyleValue): Boolean;
begin
  Result := (AStyle <> nil) and (AStyleValue in AStyle.AssignedValues);
end;

function CombineParamsWithStyle(AStyle: TcxStyle;
  AHasValues: TcxStyleValues; var AParams: TcxViewParams): TcxStyleValues;

  function CanSetStyleValue(AValue: TcxStyleValue;
    var ASetValues: TcxStyleValues): Boolean;
  begin
    Result := not (AValue in AHasValues) and (AValue in AStyle.AssignedValues);
    if Result then
      Include(ASetValues, AValue);
  end;

begin
  Result := AHasValues;
  if (AStyle = nil) or ([svBitmap..svTextColor] = AHasValues) or
    (csDestroying in AStyle.ComponentState) then Exit;
  if CanSetStyleValue(svColor, Result) then
    AParams.Color := AStyle.Color;
  if (AStyle.Font <> nil) and CanSetStyleValue(svFont, Result) then
    AParams.Font := AStyle.Font;
  if CanSetStyleValue(svTextColor, Result) then
    AParams.TextColor := AStyle.TextColor;
  if CanSetStyleValue(svBitmap, Result) then
    AParams.Bitmap := AStyle.Bitmap;
end;

{ TcxCustomStyle }

constructor TcxCustomStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList.Create;
end;

destructor TcxCustomStyle.Destroy;
begin
  StyleRepository := nil;
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TcxCustomStyle.BeforeDestruction;

  procedure RemoveNotification;
  begin
    while FListeners.Count <> 0 do
      IcxStyleChangeListener(FListeners.Last).StyleRemoved(Self);
  end;

begin
  inherited BeforeDestruction;
  RemoveNotification;
end;

procedure TcxCustomStyle.AddListener(AListener: IcxStyleChangeListener);
begin
  if FListeners.IndexOf(Pointer(AListener)) = -1 then
    FListeners.Add(Pointer(AListener));
end;

function TcxCustomStyle.GetParentComponent: TComponent;
begin
  Result := StyleRepository;
end;

function TcxCustomStyle.HasParent: Boolean;
begin
  Result := StyleRepository <> nil;
end;

procedure TcxCustomStyle.RemoveListener(AListener: IcxStyleChangeListener);
begin
  if FListeners <> nil then
    FListeners.Remove(Pointer(AListener));
end;

procedure TcxCustomStyle.RestoreDefaults;
begin
end;

procedure TcxCustomStyle.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    StyleRepository := AParent as TcxStyleRepository;
end;

procedure TcxCustomStyle.Changed;
var
  I: Integer;
begin
  for I := FListeners.Count - 1 downto 0 do
    IcxStyleChangeListener(FListeners[I]).StyleChanged(Self);
end;

procedure TcxCustomStyle.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  StyleRepository := TcxStyleRepository(Reader.Parent);
end;

procedure TcxCustomStyle.ChangeScale(M, D: Integer);
begin
//do nothing
end;

function TcxCustomStyle.GetIndex: Integer;
begin
  if StyleRepository <> nil then
    Result := StyleRepository.StyleIndexOf(self)
  else Result := -1;
end;

procedure TcxCustomStyle.SetStyleRepository(Value: TcxStyleRepository);
begin
  if FStyleRepository <> Value then
  begin
    if FStyleRepository <> nil then
      FStyleRepository.RemoveItem(Self);
    FStyleRepository := Value;
    if Value <> nil then
      Value.AddItem(Self);
  end;
end;

{ TcxCustomStyleSheet }

constructor TcxCustomStyleSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuiltIn := csDesigning in ComponentState;
  FStyles := GetStylesClass.Create(self);
  FStyles.FOwnerStyleSheet := Self;
  FStylesList := TList.Create;
end;

destructor TcxCustomStyleSheet.Destroy;
begin
  while FStylesList.Count > 0 do
    TcxCustomStyles(FStylesList[FStylesList.Count - 1]).StyleSheet := nil;
  StyleRepository := nil;
  FreeAndNil(FStyles);
  FreeAndNil(FStylesList);
  inherited Destroy;
end;

procedure TcxCustomStyleSheet.AddStyles(AStyles: TcxCustomStyles);
begin
  if FStylesList.IndexOf(AStyles) < 0 then
    FStylesList.Add(AStyles);
end;

procedure TcxCustomStyleSheet.Assign(Source: TPersistent);
begin
  if Source is TcxCustomStyleSheet then
    with TcxCustomStyleSheet(Source) do
    begin
      Self.Caption := Caption;
      Self.SetStyles(GetStyles);
    end
  else
    inherited;
end;

procedure TcxCustomStyleSheet.CopyFrom(AStyleSheet: TcxCustomStyleSheet);
var
  ASourceDPI: Integer;
  AStyle: TcxCustomStyle;
  AStyleClass: TcxCustomStyleClass;
  AStyleItem: TcxCustomStylesItem;
  ATargetDPI: Integer;
  I: Integer;
begin
  if (AStyleSheet <> nil) and (GetStylesClass = AStyleSheet.GetStylesClass) then
  begin
    ASourceDPI := dxGetScaleFactor(AStyleSheet.StyleRepository).TargetDPI;
    ATargetDPI := dxGetScaleFactor(StyleRepository).TargetDPI;

    for I := 0 to AStyleSheet.GetStyles.Count - 1 do
    begin
      AStyleItem := AStyleSheet.GetStyles.Items[I];
      AStyleClass := TcxCustomStyleClass(AStyleItem.Item.ClassType);
      if StyleRepository <> nil then
        AStyle := StyleRepository.CreateItemEx(AStyleClass, StyleRepository.Owner)
      else
        AStyle := AStyleClass.Create(Owner);

      AStyle.Assign(AStyleItem.Item);
      if ASourceDPI <> ATargetDPI then
        AStyle.ChangeScale(ATargetDPI, ASourceDPI);
      GetStyles.Values[AStyleItem.Index] := AStyle;
    end;
  end;
end;

class function TcxCustomStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxCustomStyles;
end;

function TcxCustomStyleSheet.GetParentComponent: TComponent;
begin
  Result := StyleRepository;
end;

function TcxCustomStyleSheet.HasParent: Boolean;
begin
  Result := StyleRepository <> nil;
end;

function TcxCustomStyleSheet.GetStyles: TcxCustomStyles;
begin
  Result := FStyles;
end;

procedure TcxCustomStyleSheet.RemoveStyles(AStyles: TcxCustomStyles);
begin
  FStylesList.Remove(AStyles);
end;

procedure TcxCustomStyleSheet.SetStyles(const Value: TcxCustomStyles);
begin
  FStyles.Assign(Value);
end;

procedure TcxCustomStyleSheet.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    StyleRepository := AParent as TcxStyleRepository;
end;

procedure TcxCustomStyleSheet.DefineProperties(AFiler: TFiler);
begin
  inherited;
  AFiler.DefineProperty('BuiltIn', ReadBuiltIn, WriteBuiltIn, True);
end;

procedure TcxCustomStyleSheet.DoStyleChanged(AIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FStylesList.Count - 1 do
     TcxCustomStyles(FStylesList[I]).DoChanged(AIndex);
  UpdateFakeLinks;
end;

procedure TcxCustomStyleSheet.GetFakeComponentLinks(AList: TList);
var
  I: Integer;
begin
  for I := 0 to FStyles.Count - 1 do
    if (FStyles.Items[I].Item.Owner <> Owner) and (Owner <> nil) and
      (AList.IndexOf(FStyles.Items[I].Item.Owner) < 0) then
      AList.Add(FStyles.Items[I].Item.Owner);
end;

procedure TcxCustomStyleSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  StyleRepository := TcxStyleRepository(Reader.Parent);
end;

function TcxCustomStyleSheet.GetCaptionStored: Boolean;
begin
  Result := FCaption <> '';
end;

function TcxCustomStyleSheet.GetCaption: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := Name;
end;

function TcxCustomStyleSheet.GetIndex: Integer;
begin
  if StyleRepository <> nil then
    Result := StyleRepository.StyleSheetIndexOf(self)
  else
    Result := -1;
end;

procedure TcxCustomStyleSheet.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    if CompareText(Name, Value) <> 0 then
      FCaption := Value
    else
      FCaption := '';
  end;
end;

procedure TcxCustomStyleSheet.SetStyleRepository(Value: TcxStyleRepository);
begin
  if FStyleRepository <> Value then
  begin
    if FStyleRepository <> nil then
      FStyleRepository.RemoveStyleSheet(Self);
    FStyleRepository := Value;
    if Value <> nil then
      Value.AddStyleSheet(Self);
  end;
end;

procedure TcxCustomStyleSheet.ReadBuiltIn(AReader: TReader);
begin
  FBuiltIn := AReader.ReadBoolean;
end;

procedure TcxCustomStyleSheet.WriteBuiltIn(AWriter: TWriter);
begin
  AWriter.WriteBoolean(FBuiltIn);
end;

{ TcxStyleRepository }

constructor TcxStyleRepository.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FStyleSheets := TList.Create;
end;

destructor TcxStyleRepository.Destroy;
begin
  Clear;
  ClearStyleSheets;
  FItems.Free;
  FItems := nil;
  FStyleSheets.Free;
  FStyleSheets := nil;
  inherited Destroy;
end;

procedure TcxStyleRepository.Clear;
begin
  while Count > 0 do
    Items[Count - 1].Free;
end;

procedure TcxStyleRepository.ClearStyleSheets;
begin
  while StyleSheetCount > 0 do
    StyleSheets[StyleSheetCount - 1].Free;
end;

function TcxStyleRepository.CreateItem(AStyleClass: TcxCustomStyleClass): TcxCustomStyle;
begin
  Result := CreateItemEx(AStyleClass, Self);
end;

function TcxStyleRepository.CreateItemEx(AStyleClass: TcxCustomStyleClass;
  AOwner: TComponent): TcxCustomStyle;
begin
  Result := AStyleClass.Create(AOwner);
  Result.StyleRepository := Self;
end;

function TcxStyleRepository.CreateStyleSheet(AStyleSheetClass: TcxCustomStyleSheetClass): TcxCustomStyleSheet;
begin
  Result := CreateStyleSheetEx(AStyleSheetClass, Self);
end;

function TcxStyleRepository.CreateStyleSheetEx(AStyleSheetClass: TcxCustomStyleSheetClass;
  AOwner: TComponent): TcxCustomStyleSheet;
begin
  Result := AStyleSheetClass.Create(AOwner);
  Result.StyleRepository := Self;
end;

function TcxStyleRepository.StyleIndexOf(AStyle: TcxCustomStyle): Integer;
begin
  Result := FItems.IndexOf(AStyle);
end;

function TcxStyleRepository.StyleSheetIndexOf(AStyleSheet: TcxCustomStyleSheet): Integer;
begin
  Result := FStyleSheets.IndexOf(AStyleSheet);
end;

procedure TcxStyleRepository.AddItem(AItem: TcxCustomStyle);
var
  AIndex: Integer;
begin
  AIndex := FItems.IndexOf(AItem);
  if AIndex = -1 then
    FItems.Add(AItem);
end;

procedure TcxStyleRepository.AddStyleSheet(AStyleSheet: TcxCustomStyleSheet);
var
  AIndex: Integer;
begin
  AIndex := FStyleSheets.IndexOf(AStyleSheet);
  if AIndex = -1 then
    FStyleSheets.Add(AStyleSheet);
end;

procedure TcxStyleRepository.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

procedure TcxStyleRepository.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  AItem: TcxCustomStyle;
  AStyleSheet: TcxCustomStyleSheet;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.Owner = Root then
      Proc(AItem);
  end;
  for I := 0 to StyleSheetCount - 1 do
  begin
    AStyleSheet := StyleSheets[I];
    if AStyleSheet.Owner = Root then
      Proc(AStyleSheet);
  end;
end;

procedure TcxStyleRepository.RemoveItem(AItem: TcxCustomStyle);
begin
  FItems.Remove(AItem);
end;

procedure TcxStyleRepository.RemoveStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  FStyleSheets.Remove(AStyleSheet);
end;

function TcxStyleRepository.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxStyleRepository.GetStyleSheetCount: Integer;
begin
  Result := FStyleSheets.Count;
end;

function TcxStyleRepository.GetItem(Index: Integer): TcxCustomStyle;
begin
  Result := TcxCustomStyle(FItems[Index]);
end;

function TcxStyleRepository.GetStyleSheet(Index: Integer): TcxCustomStyleSheet;
begin
  Result := TcxCustomStyleSheet(FStyleSheets[Index]);
end;

{ TcxCustomStylesItem }

constructor TcxCustomStylesItem.Create(AIndex: Integer; AItem: TcxCustomStyle);
begin
  inherited Create;
  Index := AIndex;
  Item := AItem;
end;

{ TcxCustomStyles }

constructor TcxCustomStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
end;

destructor TcxCustomStyles.Destroy;
begin
  FDestroying := True;
  Clear;
  FItems.Free;
  StyleSheet := nil;
  inherited Destroy;
end;

procedure TcxCustomStyles.Assign(Source: TPersistent);
begin
  if Source is TcxCustomStyles then
    StyleSheet := TcxCustomStyles(Source).StyleSheet;
end;

function TcxCustomStyles.IsValidStyleSheet(AStyleSheet: TcxCustomStyleSheet): Boolean;
var
  AStyles: TcxCustomStyles;
begin
  Result := (AStyleSheet = nil) or
    ((AStyleSheet <> OwnerStyleSheet) and (AStyleSheet.GetStylesClass = TcxCustomStylesClass(ClassType)));
  if Result and (AStyleSheet <> nil) and (OwnerStyleSheet <> nil) then //check for a cycle
  begin
    AStyles := AStyleSheet.GetStyles;
    while AStyles.StyleSheet <> nil do
    begin
      if AStyles.StyleSheet = OwnerStyleSheet then
      begin
        Result := False;
        break;
      end;
      AStyles := AStyles.StyleSheet.GetStyles;
    end;
  end;
end;

procedure TcxCustomStyles.StyleChanged(Sender: TcxCustomStyle);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Item = Sender then
      DoChanged(Items[I].Index);
end;

procedure TcxCustomStyles.StyleRemoved(Sender: TcxCustomStyle);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Item = Sender then
      Delete(I);
end;

procedure TcxCustomStyles.Clear;
begin
  while Count > 0 do
    Delete(Count - 1);
end;

procedure TcxCustomStyles.Delete(AItemIndex: Integer);
var
  AItem: TcxCustomStylesItem;
  AIndex: Integer;
begin
  AItem := Items[AItemIndex];
  AIndex := AItem.Index;
  AItem.Item.RemoveListener(Self);
  AItem.Free;
  FItems.Delete(AItemIndex);
  DoChanged(AIndex);
end;

procedure TcxCustomStyles.DoChanged(AIndex: Integer);
begin
  if Destroying then Exit;
  Changed(AIndex);
  if OwnerStyleSheet <> nil then
    OwnerStyleSheet.DoStyleChanged(AIndex);
end;

function TcxCustomStyles.Find(AIndex: Integer; var AItemIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  AItemIndex := 0;
  Result := False;
  L := 0;
  H := Count - 1;
  if L <= H then
    repeat
      I := (L + H) div 2;
      C := Items[I].Index - AIndex;
      if C = 0 then
      begin
        AItemIndex := I;
        Result := True;
        Break;
      end
      else
        if C < 0 then
          L := I + 1
        else
          H := I - 1;
      if L > H then
      begin
        AItemIndex := L;
        Break;
      end;
    until False;
end;

function TcxCustomStyles.GetValue(Index: Integer): TcxCustomStyle;
var
  AItemIndex: Integer;
begin
  if Find(Index, AItemIndex) then
    Result := Items[AItemIndex].Item
  else
    Result := nil;
  if (Result = nil) and (StyleSheet <> nil) and
    ((GetOwner = nil) or not (GetOwner is TComponent) or  // don't persist a stylesheet's style
    not (csWriting in TComponent(GetOwner).ComponentState)) then
    Result := StyleSheet.GetStyles.GetValue(Index);
end;

procedure TcxCustomStyles.ResetStyles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Values[I] := nil;
end;

procedure TcxCustomStyles.SetValue(Index: Integer; Value: TcxCustomStyle);
var
  AStylesRootOwner, AValueOwner: TPersistent;
  AItem: TcxCustomStylesItem;
  AItemIndex: Integer;
begin
  if (Value <> nil) and (Value.Name <> '') then
  begin
    AStylesRootOwner := Owner;
    while GetUltimateOwner(AStylesRootOwner) <> nil do
      AStylesRootOwner := GetUltimateOwner(AStylesRootOwner);
    AValueOwner := Value.Owner;
    if (AStylesRootOwner <> AValueOwner) and
      (AStylesRootOwner is TComponent) and (AValueOwner is TComponent) and
      AStylesRootOwner.InheritsFrom(AValueOwner.ClassType) then
      Value := TComponent(AStylesRootOwner).FindComponent(Value.Name) as TcxCustomStyle;
  end;
  if GetValue(Index) <> Value then
    if Find(Index, AItemIndex) then
    begin
      AItem := Items[AItemIndex];
      AItem.Item.RemoveListener(Self);
      if Value <> nil then
      begin
        AItem.Item := Value;
        Value.AddListener(Self);
      end
      else
      begin
        AItem.Free;
        FItems.Delete(AItemIndex);
      end;
      DoChanged(Index);
    end
    else
      if Value <> nil then
      begin
        AItem := TcxCustomStylesItem.Create(Index, Value);
        FItems.Insert(AItemIndex, AItem);
        Value.AddListener(Self);
        DoChanged(Index);
      end;
end;

procedure TcxCustomStyles.Changed(AIndex: Integer);
begin
end;

function TcxCustomStyles.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxCustomStyles.GetItem(Index: Integer): TcxCustomStylesItem;
begin
  Result := TcxCustomStylesItem(FItems[Index]);
end;

procedure TcxCustomStyles.SetStyleSheet(const Value: TcxCustomStyleSheet);
var
  AChangedStyles: TList;

  procedure AddChangedStyles(AStyleSheet: TcxCustomStyleSheet);
  var
    I: Integer;
    AIndex: Pointer;
  begin
    if AStyleSheet = nil then exit;
    if AChangedStyles = nil then
      AChangedStyles := TList.Create;
    for I := 0 to AStyleSheet.GetStyles.Count - 1 do
    begin
      AIndex := Pointer(AStyleSheet.GetStyles.Items[I].Index);
      if AChangedStyles.IndexOf(AIndex) < 0 then
        AChangedStyles.Add(AIndex);
    end;
    if AStyleSheet.GetStyles.StyleSheet <> nil then
      AddChangedStyles(AStyleSheet.GetStyles.StyleSheet);
  end;

var
  I: Integer;
begin
  if (StyleSheet <> Value) and IsValidStyleSheet(Value) then
  begin
    AChangedStyles := nil;
    if (StyleSheet <> nil) then
    begin
      StyleSheet.RemoveStyles(self);
      if not Destroying then
        AddChangedStyles(StyleSheet);
    end;
    FStyleSheet := Value;
    if StyleSheet <> nil then
    begin
      StyleSheet.AddStyles(self);
      AddChangedStyles(StyleSheet);
    end;
    if AChangedStyles <> nil then
    begin
      for I := 0 to AChangedStyles.Count - 1 do
        DoChanged(Integer(AChangedStyles[I]));
      AChangedStyles.Free;
    end;
  end;
end;

{ TcxStyle }

constructor TcxStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clDefault;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FTextColor := clDefault;
end;

destructor TcxStyle.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TcxStyle.SetAssignedValues(Value: TcxStyleValues);
begin
  if FAssignedValues <> Value then
  begin
    FAssignedValues := Value;
    Changed;
  end;
end;

procedure TcxStyle.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TcxStyle.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FColor = clDefault then // It's correct?
      Exclude(FAssignedValues, svColor)
    else
      Include(FAssignedValues, svColor);
    Changed;
  end;
end;

procedure TcxStyle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TcxStyle.SetTextColor(Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    if FTextColor = clDefault then // It's correct?
      Exclude(FAssignedValues, svTextColor)
    else
      Include(FAssignedValues, svTextColor);
    Changed;
  end;
end;

procedure TcxStyle.BitmapChanged(Sender: TObject);
begin
  if FBitmap.Empty then
    Exclude(FAssignedValues, svBitmap)
  else
    Include(FAssignedValues, svBitmap);
  Changed;
end;

procedure TcxStyle.FontChanged(Sender: TObject);
begin
  Include(FAssignedValues, svFont);
  Changed;
end;

function TcxStyle.IsBitmapStored: Boolean;
begin
  Result := svBitmap in FAssignedValues;
end;

function TcxStyle.IsColorStored: Boolean;
begin
  Result := svColor in FAssignedValues;
end;

function TcxStyle.IsFontStored: Boolean;
begin
  Result := svFont in FAssignedValues;
end;

function TcxStyle.IsTextColorStored: Boolean;
begin
  Result := svTextColor in FAssignedValues;
end;

procedure TcxStyle.ChangeScale(M, D: Integer);
begin
  inherited;
  if svFont in AssignedValues then
    Font.Height := MulDiv(Font.Height, M, D);
end;

procedure TcxStyle.Assign(Source: TPersistent);
begin
  if Source is TcxStyle then
    with TcxStyle(Source) do
    begin
      Self.Bitmap := Bitmap;
      Self.Color := Color;
      Self.Font := Font;
      Self.TextColor := TextColor;
      Self.AssignedValues := AssignedValues;
    end
  else
    inherited Assign(Source);
end;

procedure TcxStyle.RestoreDefaults;
begin
  FAssignedValues := [];
  FBitmap.Assign(nil);
  FColor := clDefault;
  FTextColor := clDefault;
  Changed;
end;

{ TcxStyles }

destructor TcxStyles.Destroy;
begin
  FreeAndNil(FScaledFonts);
  inherited Destroy;
end;

procedure TcxStyles.DefaultFontChanged(Sender: TObject; AFont: TFont);
var
  I: Integer;
  AParams: TcxViewParams;
begin
  for I := 0 to FMaxDefaultViewParamsIndex do
    if IsDefaultFont(I) then
    begin
      GetDefaultViewParams(I, nil, AParams);
      if AParams.Font = AFont then
        DoChanged(I);
    end;
end;

function TcxStyles.GetValue(Index: Integer): TcxStyle;
begin
  Result := TcxStyle(inherited GetValue(Index));
end;

procedure TcxStyles.SetValue(Index: Integer; Value: TcxStyle);
begin
  inherited SetValue(Index, Value);
end;

procedure TcxStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
begin
  FillChar(AParams, SizeOf(AParams), 0);
  AParams.TextColor := clDefault;
  AParams.Color := clDefault;
  if Index > FMaxDefaultViewParamsIndex then
    FMaxDefaultViewParamsIndex := Index;
end;

procedure TcxStyles.InternalGetViewParams(Index: Integer; AData: TObject; AStyle: TcxStyle; out AParams: TcxViewParams);

  function GetBitmap(AAssignedStyle: TcxStyle): TBitmap;
  begin
    Result := nil;
    if BitmapInViewParams then
    begin
      if UseStyle(AStyle, svBitmap) then
        Result := AStyle.Bitmap
      else
        if UseStyle(AAssignedStyle, svBitmap) then
          Result := AAssignedStyle.Bitmap;
    end;
  end;

  function GetColor(AAssignedStyle: TcxStyle): TColor;
  begin
    if UseStyle(AStyle, svColor) then
      Result := AStyle.Color
    else
      if UseStyle(AAssignedStyle, svColor) then
        Result := AAssignedStyle.Color
      else
        Result := clDefault;
  end;

  function GetFont(AAssignedStyle: TcxStyle): TFont;
  begin
    if UseStyle(AStyle, svFont) then
      Result := GetActualFont(AStyle)
    else
      if UseStyle(AAssignedStyle, svFont) then
        Result := GetActualFont(AAssignedStyle)
      else
        Result := nil;
  end;

  function GetTextColor(AAssignedStyle: TcxStyle): TColor;
  begin
    if UseStyle(AStyle, svTextColor) then
      Result := AStyle.TextColor
    else
      if UseStyle(AAssignedStyle, svTextColor) then
        Result := AAssignedStyle.TextColor
      else
        Result := clDefault;
  end;

var
  AAssignedStyle: TcxStyle;
begin
  AAssignedStyle := Values[Index];
  AParams.Bitmap := GetBitmap(AAssignedStyle);
  AParams.Color := GetColor(AAssignedStyle);
  AParams.Font := GetFont(AAssignedStyle);
  AParams.TextColor := GetTextColor(AAssignedStyle);
end;

function TcxStyles.IsDefaultFont(Index: Integer): Boolean;
begin
  Result := not UseStyle(Values[Index], svFont);
end;

function TcxStyles.GetBitmap(Index: Integer): TBitmap;
var
  AAssignedStyle: TcxStyle;
begin
  AAssignedStyle := Values[Index];
  if UseStyle(AAssignedStyle, svBitmap) and not AAssignedStyle.Bitmap.Empty then
    Result := AAssignedStyle.Bitmap
  else
    Result := nil;
end;

procedure TcxStyles.GetViewParams(Index: Integer; AData: TObject; AStyle: TcxStyle; out AParams: TcxViewParams);

  function NeedGetDefaultViewParams(const AParams: TcxViewParams): Boolean;
  begin
    with AParams do
      Result := BitmapInViewParams and (Bitmap = nil) or (Color = clDefault) or (Font = nil) or (TextColor = clDefault)
  end;

var
  ADefaultParams: TcxViewParams;
begin
  InternalGetViewParams(Index, AData, AStyle, AParams);
  if NeedGetDefaultViewParams(AParams) then
  begin
    GetDefaultViewParams(Index, AData, ADefaultParams);
    if AParams.Bitmap = nil then
      AParams.Bitmap := ADefaultParams.Bitmap;
    if AParams.Color = clDefault then
      AParams.Color := ADefaultParams.Color;
    if AParams.Font = nil then
      AParams.Font := ADefaultParams.Font;
    if AParams.TextColor = clDefault then
      AParams.TextColor := ADefaultParams.TextColor;
  end;
end;

function TcxStyles.GetActualFont(AStyle: TcxStyle): TFont;
var
  AOwnerScaleFactor: TdxScaleFactor;
  AStyleScaleFactor: TdxScaleFactor;
begin
  Result := AStyle.Font;
  AOwnerScaleFactor := dxGetScaleFactor(Owner);
  AStyleScaleFactor := dxGetScaleFactor(AStyle.StyleRepository);
  if not AOwnerScaleFactor.Equals(AStyleScaleFactor) then
  begin
    if FScaledFonts = nil then
      FScaledFonts := TObjectDictionary<TcxCustomStyle, TFont>.Create([doOwnsValues]);
    if not FScaledFonts.TryGetValue(AStyle, Result) then
    begin
      Result := TFont.Create;
      FScaledFonts.AddOrSetValue(AStyle, Result);
    end;
    Result.Assign(AStyle.Font);
    Result.PixelsPerInch := dxDefaultDPI;
    Result.Height := AOwnerScaleFactor.Apply(Result.Height, AStyleScaleFactor);
  end;
end;

procedure CreateStyleSheetStyles(ADestStyleSheet, ASourceStyleSheet: TcxCustomStyleSheet;
  AStyleGetName: TcxStyleGetName = nil);
var
  APropList: TPropList;
  I, ACount: Integer;
  ADestStyle, ASourceStyle: TcxStyle;
begin
  if ADestStyleSheet.GetStylesClass <> ASourceStyleSheet.GetStylesClass then Exit;
  ACount := GetPropList(ADestStyleSheet.GetStyles.ClassInfo, [tkClass], @APropList);
  for I := 0 to ACount - 1 do
    if GetTypeData(APropList[I].PropType^).ClassType = TcxStyle then
    begin
      ADestStyle := TcxStyle(GetObjectProp(ADestStyleSheet.GetStyles, dxShortStringToString(APropList[I].Name)));
      ASourceStyle := TcxStyle(GetObjectProp(ASourceStyleSheet.GetStyles, dxShortStringToString(APropList[I].Name)));
      if ASourceStyle <> nil then
      begin
        if ADestStyle <> nil then
          ADestStyle.Assign(ASourceStyle)
        else
          if (ADestStyleSheet.StyleRepository <> nil) and
            (ASourceStyle.AssignedValues <> []) then
          begin
            ADestStyle := TcxStyle(ADestStyleSheet.StyleRepository.CreateItemEx(TcxStyle, ADestStyleSheet.Owner));
            if Assigned(AStyleGetName) then
              ADestStyle.Name := AStyleGetName(ADestStyle);
            ADestStyle.Assign(ASourceStyle);
            SetObjectProp(ADestStyleSheet.GetStyles, APropList[I], ADestStyle);
          end;
      end
      else
        SetObjectProp(ADestStyleSheet.GetStyles, APropList[I], nil);
    end;
end;

procedure RegisterStyleSheetClass(AStyleSheetClass: TcxCustomStyleSheetClass);
begin
  if FStyleSheetClasses = nil then
    FStyleSheetClasses := TList.Create;
  if FStyleSheetClasses.IndexOf(TObject(AStyleSheetClass)) = -1 then
    FStyleSheetClasses.Add(TObject(AStyleSheetClass));
end;

procedure UnregisterStyleSheetClass(AStyleSheetClass: TcxCustomStyleSheetClass);
begin
  if FStyleSheetClasses <> nil then
    FStyleSheetClasses.Remove(TObject(AStyleSheetClass));
end;

procedure GetRegisteredStyleSheetClasses(AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  if FStyleSheetClasses <> nil then
    for I := 0 to FStyleSheetClasses.Count - 1 do
       AList.Add(FStyleSheetClasses[I]);
end;

function ColorToText(AColor: TColor): string;
begin
  Result := IntToStr(AColor);
end;

function TextToColor(const ASt: string): TColor;
begin
  if CompareText(ASt, DefaultValue) <> 0 then
    Result := TColor(StrToInt(ASt))
  else Result := clDefault;
end;

function FontToText(AFont: TFont): string;
begin
  Result := AFont.Name + ',' + IntToStr(AFont.Size) + ',[';
  if fsBold in AFont.Style then
    Result := Result + 'B';
  if fsItalic in AFont.Style then
    Result := Result + 'I';
  if fsUnderline in AFont.Style then
    Result := Result + 'U';
  if fsStrikeOut in AFont.Style then
    Result := Result + 'S';
  Result := Result + ']';
end;

procedure TextToFont(const AFont: TFont; const ASt: string);

   procedure SetFontSize(const AText: string);
   begin
     try
       AFont.Size := StrToInt(AText);
     except
     end;
   end;

var
  St: string;
begin
  st := ASt;
  if Pos(',', st) = 0 then
    AFont.Name := st
  else
  begin
    AFont.Name := Copy(st, 1, Pos(',', st) - 1);
    st := Copy(st, Pos(',', st) + 1, Length(st));
    if Pos(',', st) = 0 then
      SetFontSize(st)
    else
    begin
      SetFontSize(Copy(st, 1, Pos(',', st) - 1));
      st := Copy(st, Pos(',', st) + 1, Length(st));
      if Pos('B', st) > 0 then
        AFont.Style := AFont.Style + [fsBold];
      if Pos('I', st) > 0 then
        AFont.Style := AFont.Style + [fsItalic];
      if Pos('U', st) > 0 then
        AFont.Style := AFont.Style + [fsUnderline];
      if Pos('S', st) > 0 then
        AFont.Style := AFont.Style + [fsStrikeout];
    end;
  end;
end;

procedure SaveBitmapToIniFile(AIniFile: TCustomIniFile; ABitmap: TBitmap;
  const ASectionName, ABitmapName: string);
const
  AStringValueMaxLength = 2047;
var
  AStream: TMemoryStream;
  I: Integer;
  S: string;
begin
  AStream := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(AStream);
    S := BinaryStreamToString(AStream);
  finally
    AStream.Free;
  end;
  for I := 0 to (Length(S) + AStringValueMaxLength - 1) div AStringValueMaxLength - 1 do
    AIniFile.WriteString(ASectionName, ABitmapName + IntToStr(I),
      Copy(S, 1 + I * AStringValueMaxLength, AStringValueMaxLength));
end;

procedure SaveStyleSheetsToIniFile(const AIniFileName: string; const AList: TList);
var
  AIsFileEmpty: Boolean;

  procedure SaveStyleToIni(AIniFile: TCustomIniFile; const AStyleSheetCaption, AStyleName: string;
    AStyle: TcxStyle);
  var
    Value: string;
  begin
    if svColor in AStyle.AssignedValues then
      Value := ColorToText(AStyle.Color)
    else
      Value := DefaultValue;
    if Value <> '' then
      Value := Value + ',';

    if svTextColor in AStyle.AssignedValues then
      Value := Value + ColorToText(AStyle.TextColor)
    else
      Value := Value + DefaultValue;

    if svFont in AStyle.AssignedValues then
    begin
      if Value <> '' then
        Value := Value + ',';
      Value := Value + FontToText(AStyle.Font);
    end;

    if Value <> '' then
    begin
      AIniFile.WriteString(AStyleSheetCaption, AStyleName, Value);
      AIsFileEmpty := False;
    end;

    if (svBitmap in AStyle.AssignedValues) and IsGlyphAssigned(AStyle.Bitmap) then
      SaveBitmapToIniFile(AIniFile, AStyle.Bitmap, AStyleSheetCaption, AStyleName + BitmapCaption);
  end;

  procedure SaveStyleSheetToIni(AStyleSheet: TcxCustomStyleSheet; AIniFile: TCustomIniFile);
  var
    I, ACount: Integer;
    APropList: TPropList;
    AStyle: TcxStyle;
  begin
    ACount := GetPropList(AStyleSheet.GetStyles.ClassInfo, [tkClass], @APropList);
    for I := 0 to ACount - 1 do
      if GetTypeData(APropList[I].PropType^).ClassType = TcxStyle then
      begin
        AStyle := TcxStyle(GetObjectProp(AStyleSheet.GetStyles, APropList[I]));
        if AStyle <> nil then
          SaveStyleToIni(AIniFile, AStyleSheet.Caption, dxShortStringToString(APropList[I].Name), AStyle);
      end;
  end;

var
  AIniFile: TMemIniFile;
  F: TextFile;
  I: Integer;
begin
  AIsFileEmpty := True;
  try
    AIniFile := TMemIniFile.Create(AIniFileName);
    try
      for I := 0 to AList.Count - 1 do
        SaveStyleSheetToIni(TcxCustomStyleSheet(AList[I]), AIniFile);
      if not AIsFileEmpty then AIniFile.UpdateFile;
    finally
      AIniFile.Free;
    end;
  finally
    if AIsFileEmpty then
    begin
      AssignFile(F, AIniFileName);
      Rewrite(F);
      CloseFile(F);
    end;
  end;
end;

procedure LoadStyleSheetsFromIniFile(const AIniFileName: string;
  AStyleRepository: TcxStyleRepository; AStyleSheetClass: TcxCustomStyleSheetClass;
  const AStyleSheetNames: TStrings = nil; AOwner: TComponent = nil;
  const AStyleSheetList: TList = nil; AStyleGetName: TcxStyleGetName = nil);

  procedure LoadStyleFromIni(AIniFile: TCustomIniFile; AStyleSheet: TcxCustomStyleSheet;
    AStyleName: string; var AStyleNameIndex: Integer);
  var
    AIsBitmapValue: Boolean;
    AKey, S: string;
    APos: Integer;
    APropInfo: PPropInfo;
    AStream: TMemoryStream;
    AStyle: TcxStyle;
    I: Integer;
  begin
    if AStyleName = '' then
    begin
      Inc(AStyleNameIndex);
      Exit;
    end;
    APos := Pos(BitmapCaption, AStyleName);
    if APos > 0 then
    begin
      AIsBitmapValue := True;
      AStyleName := Copy(AStyleName, 1, APos - 1);
    end
    else
      AIsBitmapValue := False;

    APropInfo := GetPropInfo(PTypeInfo(AStyleSheet.GetStyles.ClassInfo), AStyleName);
    if (APropInfo <> nil) and (GetTypeData(APropInfo.PropType^).ClassType = TcxStyle) then
    begin
      if AIsBitmapValue then
      begin
        AStyle := TcxStyle(GetObjectProp(AStyleSheet.GetStyles, APropInfo));
        S := '';
        I := 0;
        repeat
          AKey := AStyleName + BitmapCaption + IntToStr(I);
          if not AIniFile.ValueExists(AStyleSheet.Caption, AKey) then
            Break;
          S := S + AIniFile.ReadString(AStyleSheet.Caption, AKey, '');
          Inc(AStyleNameIndex);
          Inc(I);
        until False;
        AStream := TMemoryStream.Create;
        try
          StringToBinaryStream(S, AStream);
          AStyle.Bitmap.LoadFromStream(AStream);
        finally
          AStream.Free;
        end;
      end
      else
      begin
        if AOwner <> nil then
          AStyle := TcxStyle(AStyleRepository.CreateItemEx(TcxStyle, AOwner))
        else AStyle := TcxStyle(AStyleRepository.CreateItem(TcxStyle));
        if Assigned(AStyleGetName) then
          AStyle.Name := AStyleGetName(AStyle);

        S := AIniFile.ReadString(AStyleSheet.Caption, AStyleName, '');
        if Pos(',', S) = 0 then
           AStyle.Color := TextToColor(S)
        else
        begin
          AStyle.Color := TextToColor(Copy(S, 1, Pos(',', S) - 1));
          S := Copy(S, Pos(',', S) + 1, Length(S));
          if Pos(',', S) = 0 then
             AStyle.TextColor := TextToColor(S)
          else
          begin
            AStyle.TextColor := TextToColor(Copy(S, 1, Pos(',', S) - 1));
            TextToFont(AStyle.Font, Copy(S, Pos(',', S) + 1, Length(S)));
          end;
        end;
        SetObjectProp(AStyleSheet.GetStyles, APropInfo, AStyle);
        Inc(AStyleNameIndex);
      end;
    end else Inc(AStyleNameIndex);
  end;

  procedure LoadStyleSheetFromIni(const AStyleSheetName: string; AIniFile: TCustomIniFile);
  var
    ANames: TStringList;
    AStyleSheet: TcxCustomStyleSheet;
    I: Integer;
  begin
    ANames := TStringList.Create;
    try
      AIniFile.ReadSection(AStyleSheetName, ANames);
      if ANames.Count > 0 then
      begin
        if AOwner <> nil then
          AStyleSheet := AStyleRepository.CreateStyleSheetEx(AStyleSheetClass, AOwner)
        else AStyleSheet := AStyleRepository.CreateStyleSheet(AStyleSheetClass);
        if AStyleSheetList <> nil then
          AStyleSheetList.Add(AStyleSheet);
        AStyleSheet.Caption := AStyleSheetName;
        I := 0;
        while I < ANames.Count do
          LoadStyleFromIni(AIniFile, AStyleSheet, ANames[I], I);
      end;
    finally
      ANames.Free;
    end;
  end;

var
  AIniFile: TMemIniFile;
  I: Integer;
  AStrings: TStringList;
begin
  AIniFile := TMemIniFile.Create(AIniFileName);
  AStrings := TStringList.Create;
  try
    if (AStyleSheetNames = nil) or (AStyleSheetNames.Count = 0) then
      AIniFile.ReadSections(AStrings)
    else
      AStrings.AddStrings(AStyleSheetNames);
    for I := 0 to AStrings.Count - 1 do
      LoadStyleSheetFromIni(AStrings[I], AIniFile);
  finally
    AStrings.Free;
    AIniFile.Free;
  end;
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TcxCustomStyle, TControl);
  GroupDescendentsWith(TcxCustomStyleSheet, TControl);
  GroupDescendentsWith(TcxStyleRepository, TControl);
  RegisterClasses([TcxStyle]);

finalization
  FreeAndNil(FStyleSheetClasses);

end.
