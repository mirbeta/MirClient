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

unit dxRichEdit.Import.Rtf.DestinationShape;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Variants, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxCoreGraphics,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.FloatingObject,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxGenerics;

type
  { TdxRtfShapeProperties }

  TdxRtfShapePropertyValue = class
  strict private
    FHasIntegerValue: Boolean;
    FHasObjectValue: Boolean;
    FHasStringValue: Boolean;
    FIntegerValue: Integer;
    FObjectValue: TObject;
    FStringValue: string;
  public
    constructor Create(const AStringValue: string); overload;
    constructor Create(AIntegerValue: Integer); overload;
    constructor Create(AObjectValue: TObject); overload;
    property HasIntegerValue: Boolean read FHasIntegerValue;
    property HasObjectValue: Boolean read FHasObjectValue;
    property HasStringValue: Boolean read FHasStringValue;
    property IntegerValue: Integer read FIntegerValue;
    property ObjectValue: TObject read FObjectValue;
    property StringValue: string read FStringValue;
  end;

  TdxRtfShapeProperties = class(TdxNamedObjectDictionary<TdxRtfShapePropertyValue>)
  public
    constructor Create; reintroduce;
    procedure Assign(Source: TdxRtfShapeProperties);
    function HasBoolProperty(const AName: string): Boolean;
    function HasIntegerProperty(const AName: string): Boolean;
    function HasStringProperty(const AName: string): Boolean;
    function HasColorProperty(const AName: string): Boolean;
    function GetIntegerPropertyValue(const AName: string): Integer;
    function GetStringPropertyValue(const AName: string): string;
    function GetBoolPropertyValue(const AName: string): Boolean;
    function GetColorPropertyValue(const AName: string): TdxAlphaColor;
  end;

  { TdxRtfFloatingObjectImportInfo }

  TdxRtfFloatingObjectImportInfo = class(TdxFloatingObjectImportInfo)
  strict private
    FLeft: Integer;
    FRight: Integer;
    FTop: Integer;
    FBottom: Integer;
    FRtfRotation: Integer;
  public
    property Left: Integer read FLeft write FLeft;
    property Right: Integer read FRight write FRight;
    property Top: Integer read FTop write FTop;
    property Bottom: Integer read FBottom write FBottom;
    property RtfRotation: Integer read FRtfRotation write FRtfRotation;
  end;

  { TdxShapeInstanceDestination }

  TdxShapeInstanceDestination = class(TdxRichEditRtfDestinationBase)
  public const
    public const DistanceFromText = 114305;
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FIsPropertiesOwner: Boolean;
    FFloatingObjectImportInfo: TdxRtfFloatingObjectImportInfo;
    FProperties: TdxRtfShapeProperties;
    FTextBoxContent: TdxTextBoxContentType;
    class procedure ShapeTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeLeftKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeRightKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeTopKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeBottomKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeZOrderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeHorizontalPositionTypePageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeHorizontalPositionTypeMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeHorizontalPositionTypeColumnKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeVerticalPositionTypePageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeVerticalPositionTypeMarginKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeVerticalPositionTypeParagraphKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeWrapTextTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeWrapTextTypeZOrderKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeWrapTextSideKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapeLockedKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ShapePropertyKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  private
    procedure ConvertToFloatingObject;

    class function ConvertFloatingObjectTextWrapSide(AParameterValue: Integer): TdxFloatingObjectTextWrapSide; static;
    class function ConvertFloatingObjectTextWrapType(AParameterValue: Integer): TdxFloatingObjectTextWrapType; static;

    function GetFloatingObject: TdxFloatingObjectProperties;
    function GetShape: TdxShape;
    function GetTextBoxContent: TdxTextBoxContentType;
    function GetTextBoxProperties: TdxTextBoxProperties;

    procedure DoShapeLeftKeyword(AParameterValue: Integer);
    procedure DoShapeTopKeyword(AParameterValue: Integer);
    procedure DoShapeRightKeyword(AParameterValue: Integer);
    procedure DoShapeBottomKeyword(AParameterValue: Integer);

    procedure ImportFloatingObjectHorizontalPositionAlignment(APropertyValue: Integer);
    procedure ImportFloatingObjectHorizontalPositionType(APropertyValue: Integer);
    procedure ImportFloatingObjectVerticalPositionAlignment(APropertyValue: Integer);
    procedure ImportFloatingObjectVerticalPositionType(APropertyValue: Integer);
    function ImportFloatingObjectRelaitveSizeHorizontalRelation(APropertyValue: Integer): TdxFloatingObjectRelativeFromHorizontal;
    function ImportFloatingObjectRelaitveSizeVerticalRelation(APropertyValue: Integer): TdxFloatingObjectRelativeFromVertical;
    procedure FinalizeInsertion;
  protected
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    class function GetThis(AImporter: TdxRtfImporter): TdxShapeInstanceDestination; static;
    procedure ProcessControlCharCore(AChar: Char); override;
    function ProcessKeywordCore(const AKeyword: string;
      AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;
    procedure ProcessCharCore(AChar: Char); override;

    function HasBoolProperty(const AName: string): Boolean;
    function HasIntegerProperty(const AName: string): Boolean;
    function HasStringProperty(const AName: string): Boolean;
    function HasColorProperty(const AName: string): Boolean;
    function GetIntegerPropertyValue(const AName: string): Integer;
    function GetStringPropertyValue(const AName: string): string;
    function GetBoolPropertyValue(const AName: string): Boolean;
    function GetColorPropertyValue(const AName: string): TdxAlphaColor;
  public
    constructor Create(AImporter: TdxRtfImporter); overload; override;
    constructor Create(AImporter: TdxRtfImporter; AFloatingObjectImportInfo: TdxRtfFloatingObjectImportInfo;
      AProperties: TdxRtfShapeProperties); reintroduce; overload;
    destructor Destroy; override;
    procedure BeforePopRtfState; override;
    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;
    class function ShouldSwapSize(ARtfAngle: Integer): Boolean; static;

    property FloatingObjectImportInfo: TdxRtfFloatingObjectImportInfo read FFloatingObjectImportInfo;
    property FloatingObject: TdxFloatingObjectProperties read GetFloatingObject;
    property Shape: TdxShape read GetShape;
    property ShapeProperties: TdxRtfShapeProperties read FProperties;
    property TextBoxProperties: TdxTextBoxProperties read GetTextBoxProperties;
    property TextBoxContent: TdxTextBoxContentType read GetTextBoxContent;
  end;

  { TdxShapeDestination }

  TdxShapeDestination = class(TdxShapeInstanceDestination)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  strict private
    class procedure ShapeInstanceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessControlCharCore(AChar: Char); override;
    function ProcessKeywordCore(const AKeyword: string;
      AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;
  end;

  { TdxShapePropertyDestination }

  TdxShapePropertyDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FIntegerPropertyNames: TdxStringList;
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  strict private
    FProperties: TdxRtfShapeProperties;
    FPropertyName: string;
  private
    class procedure PropertyNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PropertyValueKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessControlCharCore(AChar: Char); override;
    function ProcessKeywordCore(const AKeyword: string;
      AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;
    procedure ProcessCharCore(AChar: Char); override;
  public
    constructor Create(AImporter: TdxRtfImporter; AProperties: TdxRtfShapeProperties); reintroduce;
    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;
  end;

  { TdxShapeTextDestination }

  TdxShapeTextDestination = class(TdxDestinationPieceTable)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
  public
    constructor Create(AImporter: TdxRtfImporter; ATextBoxContentType: TdxTextBoxContentType); reintroduce; virtual;
  end;

  { TdxShapePropertyNameDestination }

  TdxShapePropertyNameDestination = class(TdxStringValueDestination)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  { TdxShapePropertyPIBDestination }

  TdxShapePropertyPIBDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    FImageInfo: TdxRtfImageInfo;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    procedure ProcessCharCore(AChar: Char); override;
    procedure ProcessControlCharCore(AChar: Char); override;
    function ProcessKeywordCore(const AKeyword: string;
      AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;
  public
    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;
    property ImageInfo: TdxRtfImageInfo read FImageInfo;
  end;

  { TdxShapePropertyIntegerValueDestination }

  TdxShapePropertyIntegerValueDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    FValue: Integer;
    FStrValue: string;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    procedure ProcessCharCore(AChar: Char); override;
  public
    procedure BeforePopRtfState; override;
    property Value: Integer read FValue;
  end;

  { TdxShapePropertyStringValueDestination }

  TdxShapePropertyStringValueDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    FValue: string;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    procedure ProcessCharCore(AChar: Char); override;
  public
    property Value: string read FValue;
  end;

implementation

uses
  Math, dxTypeHelpers,
  cxVariants, cxGeometry,
  dxRichEdit.Import.Rtf.DestinationPicture,
  dxRichEdit.Import.Rtf.DestinationDefault,
  dxRichEdit.Export.Rtf.Keywords;

{ TdxRtfShapePropertyValue }

constructor TdxRtfShapePropertyValue.Create(const AStringValue: string);
begin
  FHasStringValue := True;
  FStringValue := AStringValue;
end;

constructor TdxRtfShapePropertyValue.Create(AIntegerValue: Integer);
begin
  FHasIntegerValue := True;
  FIntegerValue := AIntegerValue;
end;

constructor TdxRtfShapePropertyValue.Create(AObjectValue: TObject);
begin
  FHasObjectValue := True;
  FObjectValue := AObjectValue;
end;

{ TdxRtfShapeProperties }

function TdxRtfShapeProperties.HasBoolProperty(const AName: string): Boolean;
begin
  Result := HasIntegerProperty(AName);
end;

function TdxRtfShapeProperties.HasIntegerProperty(const AName: string): Boolean;
begin
  Result := ContainsKey(AName) and Items[AName].HasIntegerValue;
end;

function TdxRtfShapeProperties.HasStringProperty(const AName: string): Boolean;
begin
  Result := ContainsKey(AName) and Items[AName].HasStringValue;
end;

function TdxRtfShapeProperties.HasColorProperty(const AName: string): Boolean;
begin
  Result := HasIntegerProperty(AName);
end;

function TdxRtfShapeProperties.GetIntegerPropertyValue(const AName: string): Integer;
begin
  Result := Items[AName].IntegerValue;
end;

function TdxRtfShapeProperties.GetStringPropertyValue(const AName: string): string;
begin
  Result := Items[AName].StringValue;
end;

procedure TdxRtfShapeProperties.Assign(Source: TdxRtfShapeProperties);
var
  AKey: string;
begin
  Clear;
  for AKey in Source.Keys do
    AddOrSetValue(AKey, Source[AKey]);
end;

constructor TdxRtfShapeProperties.Create;
begin
  inherited Create(True);
end;

function TdxRtfShapeProperties.GetBoolPropertyValue(const AName: string): Boolean;
begin
  Result := GetIntegerPropertyValue(AName) <> 0;
end;

function TdxRtfShapeProperties.GetColorPropertyValue(const AName: string): TdxAlphaColor;
var
  AColorValue: Integer;
begin
  AColorValue := GetIntegerPropertyValue(AName);
  Result := TdxAlphaColors.FromArgb(
     255,
     AColorValue and $000000FF,
    (AColorValue and $0000FF00) shr 8,
    (AColorValue and $00FF0000) shr 16);
end;

{ TdxShapeInstanceDestination }

constructor TdxShapeInstanceDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FFloatingObjectImportInfo := TdxRtfFloatingObjectImportInfo.Create(AImporter.PieceTable);
  FTextBoxContent := TdxTextBoxContentType.Create(AImporter.DocumentModel);
  FFloatingObjectImportInfo.TextBoxContent := FTextBoxContent;
  FProperties := TdxRtfShapeProperties.Create;
  FIsPropertiesOwner := True;
end;

constructor TdxShapeInstanceDestination.Create(AImporter: TdxRtfImporter;
  AFloatingObjectImportInfo: TdxRtfFloatingObjectImportInfo; AProperties: TdxRtfShapeProperties);
begin
  inherited Create(AImporter);
  FFloatingObjectImportInfo := AFloatingObjectImportInfo;
  FProperties := AProperties;
end;

destructor TdxShapeInstanceDestination.Destroy;
begin
  if FIsPropertiesOwner then
  begin
    FFloatingObjectImportInfo.TextBoxContent := nil;
    FreeAndNil(FFloatingObjectImportInfo);
    FreeAndNil(FProperties);
  end;
  inherited Destroy;
end;

class function TdxShapeInstanceDestination.ConvertFloatingObjectTextWrapSide(
  AParameterValue: Integer): TdxFloatingObjectTextWrapSide;
begin
  if (AParameterValue < Ord(Low(TdxFloatingObjectTextWrapSide))) and (AParameterValue > Ord(Low(TdxFloatingObjectTextWrapSide))) then
    Result := TdxFloatingObjectTextWrapSide.Both
  else
    Result := TdxFloatingObjectTextWrapSide(AParameterValue);
end;

class function TdxShapeInstanceDestination.ConvertFloatingObjectTextWrapType(AParameterValue: Integer): TdxFloatingObjectTextWrapType;
var
  AKey: TdxFloatingObjectTextWrapType;
begin
  for AKey := Low(TdxRtfExportSR.FloatingObjectTextWrapTypeTable) to High(TdxRtfExportSR.FloatingObjectTextWrapTypeTable) do
    if AParameterValue = TdxRtfExportSR.FloatingObjectTextWrapTypeTable[AKey] then
      Exit(AKey);
  Result := TdxFloatingObjectTextWrapType.None;
end;

procedure TdxShapeInstanceDestination.ConvertToFloatingObject;
begin
  FloatingObjectImportInfo.IsFloatingObject := True;
end;

function TdxShapeInstanceDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxShapeInstanceDestination.Create(Importer, FFloatingObjectImportInfo, FProperties);
end;

class function TdxShapeInstanceDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxShapeInstanceDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxShapeInstanceDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxShapeInstanceDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('bin', BinKeywordHandler);
  Result.Add('shptxt', ShapeTextKeywordHandler);
  Result.Add('shpleft', ShapeLeftKeywordHandler);
  Result.Add('shpright', ShapeRightKeywordHandler);
  Result.Add('shptop', ShapeTopKeywordHandler);
  Result.Add('shpbottom', ShapeBottomKeywordHandler);
  Result.Add('shpz', ShapeZOrderKeywordHandler);
  Result.Add('shpbxpage', ShapeHorizontalPositionTypePageKeywordHandler);
  Result.Add('shpbxmargin', ShapeHorizontalPositionTypeMarginKeywordHandler);
  Result.Add('shpbxcolumn', ShapeHorizontalPositionTypeColumnKeywordHandler);
  Result.Add('shpbypage', ShapeVerticalPositionTypePageKeywordHandler);
  Result.Add('shpbymargin', ShapeVerticalPositionTypeMarginKeywordHandler);
  Result.Add('shpbypara', ShapeVerticalPositionTypeParagraphKeywordHandler);
  Result.Add('shpwr', ShapeWrapTextTypeKeywordHandler);
  Result.Add('shpfblwtxt', ShapeWrapTextTypeZOrderKeywordHandler);
  Result.Add('shpwrk', ShapeWrapTextSideKeywordHandler);
  Result.Add('shplockanchor', ShapeLockedKeywordHandler);
  Result.Add('sp', ShapePropertyKeywordHandler);
end;

function TdxShapeInstanceDestination.GetFloatingObject: TdxFloatingObjectProperties;
begin
  Result := FloatingObjectImportInfo.FloatingObjectProperties;
end;

function TdxShapeInstanceDestination.GetShape: TdxShape;
begin
  Result := FloatingObjectImportInfo.Shape;
end;

function TdxShapeInstanceDestination.GetTextBoxContent: TdxTextBoxContentType;
begin
  Result := FloatingObjectImportInfo.TextBoxContent;
end;

function TdxShapeInstanceDestination.GetTextBoxProperties: TdxTextBoxProperties;
begin
  Result := FloatingObjectImportInfo.TextBoxProperties;
end;

procedure TdxShapeInstanceDestination.DoShapeLeftKeyword(AParameterValue: Integer);
begin
  FloatingObjectImportInfo.Left := AParameterValue;
  ConvertToFloatingObject;
end;

procedure TdxShapeInstanceDestination.DoShapeTopKeyword(AParameterValue: Integer);
begin
  FloatingObjectImportInfo.Top := AParameterValue;
  ConvertToFloatingObject;
end;

procedure TdxShapeInstanceDestination.FinalizeInsertion;
begin
  TdxDefaultDestination.ResetParagraphPropertiesKeywordHandler(Importer, 0, False);
  if not FloatingObjectImportInfo.IsFloatingObject then
    Importer.InsertParagraph;
end;

procedure TdxShapeInstanceDestination.DoShapeRightKeyword(AParameterValue: Integer);
begin
  FloatingObjectImportInfo.Right := AParameterValue;
  ConvertToFloatingObject;
end;

procedure TdxShapeInstanceDestination.DoShapeBottomKeyword(AParameterValue: Integer);
begin
  FloatingObjectImportInfo.Bottom := AParameterValue;
  ConvertToFloatingObject;
end;

function TdxShapeInstanceDestination.HasBoolProperty(const AName: string): Boolean;
begin
  Result := FProperties.HasBoolProperty(AName);
end;

function TdxShapeInstanceDestination.HasIntegerProperty(const AName: string): Boolean;
begin
  Result := FProperties.HasIntegerProperty(AName);
end;

function TdxShapeInstanceDestination.HasStringProperty(const AName: string): Boolean;
begin
  Result := FProperties.HasStringProperty(AName);
end;

function TdxShapeInstanceDestination.HasColorProperty(const AName: string): Boolean;
begin
  Result := FProperties.HasColorProperty(AName);
end;

function TdxShapeInstanceDestination.GetIntegerPropertyValue(const AName: string): Integer;
begin
  Result := FProperties.GetIntegerPropertyValue(AName);
end;

function TdxShapeInstanceDestination.GetStringPropertyValue(const AName: string): string;
begin
  Result := FProperties.GetStringPropertyValue(AName);
end;

function TdxShapeInstanceDestination.GetBoolPropertyValue(const AName: string): Boolean;
begin
  Result := FProperties.GetBoolPropertyValue(AName);
end;

function TdxShapeInstanceDestination.GetColorPropertyValue(const AName: string): TdxAlphaColor;
begin
  Result := FProperties.GetColorPropertyValue(AName);
end;

class function TdxShapeInstanceDestination.GetThis(
  AImporter: TdxRtfImporter): TdxShapeInstanceDestination;
begin
  Result := TdxShapeInstanceDestination(AImporter.Destination);
end;

procedure TdxShapeInstanceDestination.ImportFloatingObjectHorizontalPositionAlignment(
  APropertyValue: Integer);
begin
  if (APropertyValue >= Ord(Low(TdxFloatingObjectHorizontalPositionAlignment))) and
    (APropertyValue <= Ord(High(TdxFloatingObjectHorizontalPositionAlignment))) then
    FloatingObject.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment(APropertyValue);
end;

procedure TdxShapeInstanceDestination.ImportFloatingObjectHorizontalPositionType(
  APropertyValue: Integer);
var
  AKey: TdxFloatingObjectHorizontalPositionType;
begin
  for AKey := Low(TdxFloatingObjectHorizontalPositionType) to High(TdxFloatingObjectHorizontalPositionType) do
    if APropertyValue = TdxRtfExportSR.FloatingObjectHorizontalPositionTypeTable[AKey] then
    begin
      FloatingObject.HorizontalPositionType := AKey;
      Exit;
    end;
end;

function TdxShapeInstanceDestination.ImportFloatingObjectRelaitveSizeHorizontalRelation(
  APropertyValue: Integer): TdxFloatingObjectRelativeFromHorizontal;
begin
  if (APropertyValue >= Ord(Low(TdxFloatingObjectRelativeFromHorizontal))) and
    (APropertyValue <= Ord(High(TdxFloatingObjectRelativeFromHorizontal))) then
    Result := TdxFloatingObjectRelativeFromHorizontal(APropertyValue)
  else
    Result := TdxFloatingObjectRelativeFromHorizontal.Margin;
end;

function TdxShapeInstanceDestination.ImportFloatingObjectRelaitveSizeVerticalRelation(
  APropertyValue: Integer): TdxFloatingObjectRelativeFromVertical;
begin
  if (APropertyValue >= Ord(Low(TdxFloatingObjectRelativeFromVertical))) and
    (APropertyValue <= Ord(High(TdxFloatingObjectRelativeFromVertical))) then
    Result := TdxFloatingObjectRelativeFromVertical(APropertyValue)
  else
    Result := TdxFloatingObjectRelativeFromVertical.Margin;
end;

procedure TdxShapeInstanceDestination.ImportFloatingObjectVerticalPositionAlignment(
  APropertyValue: Integer);
begin
  if (APropertyValue >= Ord(Low(TdxFloatingObjectVerticalPositionAlignment))) and
    (APropertyValue <= Ord(High(TdxFloatingObjectVerticalPositionAlignment))) then
    FloatingObject.VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment(APropertyValue);
end;

procedure TdxShapeInstanceDestination.ImportFloatingObjectVerticalPositionType(
  APropertyValue: Integer);
var
  AKey: TdxFloatingObjectVerticalPositionType;
begin
  for AKey := Low(TdxFloatingObjectVerticalPositionType) to High(TdxFloatingObjectVerticalPositionType) do
    if APropertyValue = TdxRtfExportSR.FloatingObjectVerticalPositionTypeTable[AKey] then
    begin
      FloatingObject.VerticalPositionType := AKey;
      Exit;
    end;
end;

procedure TdxShapeInstanceDestination.NestedGroupFinished(
  ADestination: TdxRichEditRtfDestinationBase);
begin
  if ADestination is TdxDestinationPieceTable then
  begin
    if ADestination.PieceTable <> PieceTable then
      TdxDestinationPieceTable(ADestination).FinalizePieceTableCreation;
  end;
end;

class function TdxShapeInstanceDestination.ShouldSwapSize(ARtfAngle: Integer): Boolean;
begin
  ARtfAngle := ARtfAngle mod (360 * 65536);
  if ARtfAngle < 0 then
    Inc(ARtfAngle, 360 * 65536);
  Result := ((ARtfAngle >= 45 * 65536) and (ARtfAngle < 135 * 65536)) or ((ARtfAngle >= 225 * 65536) and (ARtfAngle < 315 * 65536));
end;

procedure TdxShapeInstanceDestination.ProcessCharCore(AChar: Char);
begin
//do nothing
end;

procedure TdxShapeInstanceDestination.ProcessControlCharCore(AChar: Char);
begin
//do nothing
end;

function TdxShapeInstanceDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
var
  ATranslator: TdxTranslateKeywordEvent;
begin
  Result := KeywordHT.TryGetValue(AKeyword, ATranslator);
  if Result then
    ATranslator(Importer, AParameterValue, AHasParameter);
end;

procedure TdxShapeInstanceDestination.BeforePopRtfState;
var
  APercentageWidth, APercentageHeight, ARawRotation, ALeft, ATop, AWidth, AHeight: Integer;
  ARelativeFromHorizontal: TdxFloatingObjectRelativeFromHorizontal;
  ARelativeFromVertical: TdxFloatingObjectRelativeFromVertical;
  AName: string;
  AImageInfo: TdxRtfImageInfo;
begin
  if FloatingObjectImportInfo.ShouldIgnore then
    Exit;

  if HasIntegerProperty('shapeType') then
    FloatingObjectImportInfo.ShapeType := TdxShapeType(GetIntegerPropertyValue('shapeType'));
  if HasIntegerProperty('posh') then
    ImportFloatingObjectHorizontalPositionAlignment(GetIntegerPropertyValue('posh'));
  if HasIntegerProperty('posrelh') then
    ImportFloatingObjectHorizontalPositionType(GetIntegerPropertyValue('posrelh'));
  if HasIntegerProperty('pctHorizPos') then
    FloatingObject.PercentOffsetX := GetIntegerPropertyValue('pctHorizPos') * 100;
  if HasIntegerProperty('pctVertPos') then
    FloatingObject.PercentOffsetY := GetIntegerPropertyValue('pctVertPos') * 100;
  if HasIntegerProperty('posv') then
    ImportFloatingObjectVerticalPositionAlignment(GetIntegerPropertyValue('posv'));
  if HasIntegerProperty('posrelv') then
    ImportFloatingObjectVerticalPositionType(GetIntegerPropertyValue('posrelv'));
  if HasBoolProperty('fLayoutInCell') then
    FloatingObject.LayoutInTableCell := GetBoolPropertyValue('fLayoutInCell');
  if HasBoolProperty('fAllowOverlap') then
    FloatingObject.AllowOverlap := GetBoolPropertyValue('fAllowOverlap');
  if HasBoolProperty('fBehindDocument') then
    FloatingObject.IsBehindDoc := GetBoolPropertyValue('fBehindDocument');
  if HasBoolProperty('fPseudoInline') then
    FloatingObject.PseudoInline := GetBoolPropertyValue('fPseudoInline');
  if HasBoolProperty('fHidden') then
    FloatingObject.Hidden := GetBoolPropertyValue('fHidden');
  if HasIntegerProperty('dxWrapDistLeft') then
    FloatingObject.LeftDistance := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dxWrapDistLeft'))
  else
    FloatingObject.LeftDistance := Importer.UnitConverter.EmuToModelUnits(DistanceFromText);
  if HasIntegerProperty('dxWrapDistRight') then
    FloatingObject.RightDistance := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dxWrapDistRight'))
  else
    FloatingObject.RightDistance := Importer.UnitConverter.EmuToModelUnits(DistanceFromText);
  if HasIntegerProperty('dyWrapDistTop') then
    FloatingObject.TopDistance := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dyWrapDistTop'));
  if HasIntegerProperty('dyWrapDistBottom') then
    FloatingObject.BottomDistance := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dyWrapDistBottom'));
  if HasBoolProperty('fLockAspectRatio') then
    FloatingObject.LockAspectRatio := GetBoolPropertyValue('fLockAspectRatio');

  ARawRotation := 0;
  if HasIntegerProperty('rotation') then
  begin
    ARawRotation := GetIntegerPropertyValue('rotation');
    Shape.Rotation := Importer.UnitConverter.FDToModelUnits(ARawRotation);
  end;

  ALeft := Importer.UnitConverter.TwipsToModelUnits(FloatingObjectImportInfo.Left);
  ATop := Importer.UnitConverter.TwipsToModelUnits(FloatingObjectImportInfo.Top);
  AWidth := Math.Max(1, Importer.UnitConverter.TwipsToModelUnits(FloatingObjectImportInfo.Right - FloatingObjectImportInfo.Left));
  AHeight := Math.Max(1, Importer.UnitConverter.TwipsToModelUnits(FloatingObjectImportInfo.Bottom - FloatingObjectImportInfo.Top));

  if ShouldSwapSize(ARawRotation) then
  begin
    Dec(ALeft, AWidth);
    Inc(ATop, AWidth);
    FloatingObject.ActualSize := TSize.Create(AHeight, AWidth);
  end
  else
    FloatingObject.ActualSize := TSize.Create(AWidth, AHeight);
  FloatingObject.Offset := TPoint.Create(ALeft, ATop);

  if HasIntegerProperty('dxTextLeft') then
    TextBoxProperties.LeftMargin := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dxTextLeft'));
  if HasIntegerProperty('dxTextRight') then
    TextBoxProperties.RightMargin := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dxTextRight'));
  if HasIntegerProperty('dyTextTop') then
    TextBoxProperties.TopMargin := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dyTextTop'));
  if HasIntegerProperty('dyTextBottom') then
    TextBoxProperties.BottomMargin := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('dyTextBottom'));
  if HasBoolProperty('fFitShapeToText') then
    TextBoxProperties.ResizeShapeToFitText := GetBoolPropertyValue('fFitShapeToText');
  if HasIntegerProperty('WrapText') then
    TextBoxProperties.WrapText := GetIntegerPropertyValue('WrapText') <> 2;

  if HasIntegerProperty('pctHoriz') then
  begin
    APercentageWidth := GetIntegerPropertyValue('pctHoriz') * 100;
    if HasIntegerProperty('sizerelh') then
    begin
      ARelativeFromHorizontal := ImportFloatingObjectRelaitveSizeHorizontalRelation(GetIntegerPropertyValue('sizerelh'));
      FloatingObject.RelativeWidth := TdxFloatingObjectRelativeWidth.Create(ARelativeFromHorizontal, APercentageWidth);
    end
    else
      FloatingObject.RelativeWidth := TdxFloatingObjectRelativeWidth.Create(TdxFloatingObjectRelativeFromHorizontal.Page, APercentageWidth);
  end;
  if HasIntegerProperty('pctVert') then
  begin
    APercentageHeight := GetIntegerPropertyValue('pctVert') * 100;
    if HasIntegerProperty('sizerelv') then
    begin
      ARelativeFromVertical := ImportFloatingObjectRelaitveSizeVerticalRelation(GetIntegerPropertyValue('sizerelv'));
      FloatingObject.RelativeHeight := TdxFloatingObjectRelativeHeight.Create(ARelativeFromVertical, APercentageHeight);
    end
    else
      FloatingObject.RelativeHeight := TdxFloatingObjectRelativeHeight.Create(TdxFloatingObjectRelativeFromVertical.Page, APercentageHeight);
  end;

  if not HasBoolProperty('fLine') or GetBoolPropertyValue('fLine') then
  begin
    if HasIntegerProperty('lineWidth') then
      Shape.OutlineWidth := Importer.UnitConverter.EmuToModelUnits(GetIntegerPropertyValue('lineWidth'))
    else
      if FloatingObjectImportInfo.ShapeType <> TdxShapeType.PictureFrame then
        Shape.OutlineWidth := Importer.UnitConverter.EmuToModelUnits(9525);

    if HasColorProperty('lineColor') then
      Shape.OutlineColor := GetColorPropertyValue('lineColor')
    else
      if FloatingObjectImportInfo.ShapeType <> TdxShapeType.PictureFrame then
        Shape.OutlineColor := TdxAlphaColors.Black;
  end;
  if (not HasBoolProperty('fFilled') or GetBoolPropertyValue('fFilled')) and HasColorProperty('fillColor') then
    Shape.FillColor := GetColorPropertyValue('fillColor');

  if HasStringProperty('wzName') then
  begin
    AName := GetStringPropertyValue('wzName');
    if AName <> '' then
      FloatingObjectImportInfo.Name := AName;
  end;

  if FProperties.ContainsKey('pib') and FProperties['pib'].HasObjectValue and (FProperties['pib'].ObjectValue is TdxRtfImageInfo) then
  begin
    AImageInfo := TdxRtfImageInfo(FProperties['pib'].ObjectValue);
    try
      FloatingObjectImportInfo.Image := AImageInfo.RtfImage;
      if not FloatingObjectImportInfo.InsertFloatingObject(Importer.Position) then
        TdxDefaultDestination.InsertImage(Importer, AImageInfo);
      FinalizeInsertion;
      FloatingObjectImportInfo.ShouldIgnore := True;
    finally
      AImageInfo.Free;
    end;
  end
  else
    if (FloatingObjectImportInfo.ShapeType = TdxShapeType.TextBox) and (TextBoxContent <> nil) then
    begin
      FloatingObjectImportInfo.InsertFloatingObject(Importer.Position);
      FinalizeInsertion;
      FloatingObjectImportInfo.ShouldIgnore := True;
    end;
end;

class procedure TdxShapeInstanceDestination.ShapeBottomKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    GetThis(AImporter).DoShapeBottomKeyword(AParameterValue);
end;

class procedure TdxShapeInstanceDestination.ShapeRightKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    GetThis(AImporter).DoShapeRightKeyword(AParameterValue);
end;

class procedure TdxShapeInstanceDestination.ShapeHorizontalPositionTypeColumnKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Column;
end;

class procedure TdxShapeInstanceDestination.ShapeHorizontalPositionTypeMarginKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Margin;
end;

class procedure TdxShapeInstanceDestination.ShapeHorizontalPositionTypePageKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Page;
end;

class procedure TdxShapeInstanceDestination.ShapeLockedKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.Locked := True;
end;

class procedure TdxShapeInstanceDestination.ShapeLeftKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    GetThis(AImporter).DoShapeLeftKeyword(AParameterValue);
end;

class procedure TdxShapeInstanceDestination.ShapeTopKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    GetThis(AImporter).DoShapeTopKeyword(AParameterValue);
end;

class procedure TdxShapeInstanceDestination.ShapePropertyKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxShapePropertyDestination.Create(AImporter, GetThis(AImporter).FProperties);
end;

class procedure TdxShapeInstanceDestination.ShapeTextKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxShapeInstanceDestination;
begin
  ADestination := GetThis(AImporter);
  ADestination.FProperties.AddOrSetValue('shapeType', TdxRtfShapePropertyValue.Create(Ord(TdxShapeType.TextBox)));
  ADestination.ConvertToFloatingObject;
  AImporter.Destination := TdxShapeTextDestination.Create(AImporter, ADestination.TextBoxContent);
end;

class procedure TdxShapeInstanceDestination.ShapeVerticalPositionTypeMarginKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Margin;
end;

class procedure TdxShapeInstanceDestination.ShapeVerticalPositionTypePageKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Page;
end;

class procedure TdxShapeInstanceDestination.ShapeVerticalPositionTypeParagraphKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Paragraph;
end;

class procedure TdxShapeInstanceDestination.ShapeWrapTextSideKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
    GetThis(AImporter).FloatingObject.TextWrapSide := ConvertFloatingObjectTextWrapSide(AParameterValue);
end;

class procedure TdxShapeInstanceDestination.ShapeWrapTextTypeKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ANewWrapType: TdxFloatingObjectTextWrapType;
begin
  if AHasParameter then
  begin
    ANewWrapType := ConvertFloatingObjectTextWrapType(AParameterValue);
    if ANewWrapType <> TdxFloatingObjectTextWrapType.None then
      GetThis(AImporter).FloatingObject.TextWrapType := ANewWrapType;
  end;
end;

class procedure TdxShapeInstanceDestination.ShapeWrapTextTypeZOrderKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
  begin
    if GetThis(AImporter).FloatingObject.TextWrapType = TdxFloatingObjectTextWrapType.None then
      GetThis(AImporter).FloatingObject.IsBehindDoc := AParameterValue <> 0;
  end;
end;

class procedure TdxShapeInstanceDestination.ShapeZOrderKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FloatingObject.ZOrder := AParameterValue;
end;

{ TdxShapeDestination }

class constructor TdxShapeDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxShapeDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxShapeDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxShapeInstanceDestination.CreateKeywordHT;
  Result.Add('shpinst', ShapeInstanceKeywordHandler);
end;

class function TdxShapeDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

procedure TdxShapeDestination.ProcessControlCharCore(AChar: Char);
begin
//do nothing
end;

function TdxShapeDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
var
  ATranslator: TdxTranslateKeywordEvent;
begin
  Result := KeywordHT.TryGetValue(AKeyword, ATranslator);
  if Result then
    ATranslator(Importer, AParameterValue, AHasParameter);
end;

function TdxShapeDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxShapeDestination.Create(Importer, FloatingObjectImportInfo, ShapeProperties);
end;

class procedure TdxShapeDestination.ShapeInstanceKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxShapeInstanceDestination;
begin
  ADestination := TdxShapeInstanceDestination.GetThis(AImporter);
  AImporter.Destination := TdxShapeInstanceDestination.Create(AImporter,
    ADestination.FloatingObjectImportInfo,
    ADestination.ShapeProperties);
end;

{ TdxShapePropertyDestination }

constructor TdxShapePropertyDestination.Create(AImporter: TdxRtfImporter;
  AProperties: TdxRtfShapeProperties);
begin
  inherited Create(AImporter);
  FProperties := AProperties;
end;

class constructor TdxShapePropertyDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
  FIntegerPropertyNames := TdxStringList.Create;
  FIntegerPropertyNames.Add('posh');
  FIntegerPropertyNames.Add('posrelh');
  FIntegerPropertyNames.Add('posv');
  FIntegerPropertyNames.Add('posrelv');
  FIntegerPropertyNames.Add('fLayoutInCell');
  FIntegerPropertyNames.Add('fAllowOverlap');
  FIntegerPropertyNames.Add('fBehindDocument');
  FIntegerPropertyNames.Add('fPseudoInline');
  FIntegerPropertyNames.Add('fHidden');
  FIntegerPropertyNames.Add('dxWrapDistLeft');
  FIntegerPropertyNames.Add('dxWrapDistRight');
  FIntegerPropertyNames.Add('dyWrapDistTop');
  FIntegerPropertyNames.Add('dyWrapDistBottom');
  FIntegerPropertyNames.Add('dxTextLeft');
  FIntegerPropertyNames.Add('dxTextRight');
  FIntegerPropertyNames.Add('dyTextTop');
  FIntegerPropertyNames.Add('dyTextBottom');
  FIntegerPropertyNames.Add('fFitShapeToText');
  FIntegerPropertyNames.Add('fRotateText');
  FIntegerPropertyNames.Add('WrapText');
  FIntegerPropertyNames.Add('fLockAspectRatio');
  FIntegerPropertyNames.Add('fillColor');
  FIntegerPropertyNames.Add('fLine');
  FIntegerPropertyNames.Add('fFilled');
  FIntegerPropertyNames.Add('lineWidth');
  FIntegerPropertyNames.Add('lineColor');
  FIntegerPropertyNames.Add('rotation');
  FIntegerPropertyNames.Add('pctHoriz');
  FIntegerPropertyNames.Add('pctVert');
  FIntegerPropertyNames.Add('sizerelh');
  FIntegerPropertyNames.Add('sizerelv');
  FIntegerPropertyNames.Add('pctHorizPos');
  FIntegerPropertyNames.Add('pctVertPos');
  FIntegerPropertyNames.Add('shapeType');
end;

procedure TdxShapePropertyDestination.NestedGroupFinished(
  ADestination: TdxRichEditRtfDestinationBase);
var
  AStrValue: string;
begin
  if ADestination is TdxShapePropertyNameDestination then
  begin
    AStrValue := TdxShapePropertyNameDestination(ADestination).Value;
    if AStrValue <> '' then
      FPropertyName := AStrValue;
  end;

  if ADestination is TdxShapePropertyPIBDestination then
  begin
    if FPropertyName <> '' then
      FProperties.AddOrSetValue(FPropertyName, TdxRtfShapePropertyValue.Create(TdxShapePropertyPIBDestination(ADestination).ImageInfo));
  end;

  if ADestination is TdxShapePropertyIntegerValueDestination then
  begin
    if FPropertyName <> '' then
      FProperties.AddOrSetValue(FPropertyName, TdxRtfShapePropertyValue.Create(TdxShapePropertyIntegerValueDestination(ADestination).Value));
  end;

  if ADestination is TdxShapePropertyStringValueDestination then
  begin
    if FPropertyName <> '' then
      FProperties.AddOrSetValue(FPropertyName, TdxRtfShapePropertyValue.Create(TdxShapePropertyStringValueDestination(ADestination).Value));
  end;
end;

procedure TdxShapePropertyDestination.ProcessCharCore(AChar: Char);
begin
//do nothing
end;

procedure TdxShapePropertyDestination.ProcessControlCharCore(AChar: Char);
begin
//do nothing
end;

function TdxShapePropertyDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
var
  ATranslator: TdxTranslateKeywordEvent;
begin
  Result := KeywordHT.TryGetValue(AKeyword, ATranslator);
  if Result then
    ATranslator(Importer, AParameterValue, AHasParameter);
end;

class procedure TdxShapePropertyDestination.PropertyNameKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxShapePropertyNameDestination.Create(AImporter);
end;

class procedure TdxShapePropertyDestination.PropertyValueKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxShapePropertyDestination;
  AName: string;
begin
  ADestination := TdxShapePropertyDestination(AImporter.Destination);
  AName := ADestination.FPropertyName;
  if AName = '' then
    Exit;
  if AName = 'pib' then
    AImporter.Destination := TdxShapePropertyPIBDestination.Create(AImporter)
  else
    if FIntegerPropertyNames.Contains(AName) then
      AImporter.Destination := TdxShapePropertyIntegerValueDestination.Create(AImporter)
    else
      AImporter.Destination := TdxShapePropertyStringValueDestination.Create(AImporter);
end;

function TdxShapePropertyDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxShapePropertyDestination.Create(Importer, FProperties);
  TdxShapePropertyDestination(Result).FPropertyName := FPropertyName;
end;

class function TdxShapePropertyDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxShapePropertyDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('bin', BinKeywordHandler);
  Result.Add('sn', PropertyNameKeywordHandler);
  Result.Add('sv', PropertyValueKeywordHandler);
end;

class destructor TdxShapePropertyDestination.Finalize;
begin
  FreeAndNil(FIntegerPropertyNames);
  FreeAndNil(FKeywordHT);
end;

{ TdxShapeTextDestination }

constructor TdxShapeTextDestination.Create(AImporter: TdxRtfImporter;
  ATextBoxContentType: TdxTextBoxContentType);
begin
  inherited Create(AImporter, TdxPieceTable(ATextBoxContentType.PieceTable));
end;

class constructor TdxShapeTextDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxShapeTextDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

function TdxShapeTextDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxShapeTextDestination.Create(Importer, TdxTextBoxContentType(PieceTable.ContentType));
end;

class function TdxShapeTextDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxShapeTextDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  AddCommonCharacterKeywords(Result);
  AddCommonParagraphKeywords(Result);
  AddCommonSymbolsAndObjectsKeywords(Result);
  AddCommonTabKeywords(Result);
  AddCommonNumberingListsKeywords(Result);
  AppendTableKeywords(Result);
end;

{ TdxShapePropertyPIBDestination }

function TdxShapePropertyPIBDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxShapePropertyPIBDestination.Create(Importer);
  TdxShapePropertyPIBDestination(Result).FImageInfo := ImageInfo;
end;

procedure TdxShapePropertyPIBDestination.NestedGroupFinished(
  ADestination: TdxRichEditRtfDestinationBase);
begin
  if ADestination is TdxPictureDestination then
    FImageInfo := TdxPictureDestination(ADestination).GetImageInfo;
end;

procedure TdxShapePropertyPIBDestination.ProcessCharCore(AChar: Char);
begin
//do nothing
end;

procedure TdxShapePropertyPIBDestination.ProcessControlCharCore(AChar: Char);
begin
//do nothing
end;

function TdxShapePropertyPIBDestination.ProcessKeywordCore(
  const AKeyword: string; AParameterValue: Integer;
  AHasParameter: Boolean): Boolean;
begin
  Result := AKeyword = 'pict';
  if Result then
    Importer.Destination := TdxPictureDestination.Create(Importer);
end;

{ TdxShapePropertyIntegerValueDestination }

procedure TdxShapePropertyIntegerValueDestination.BeforePopRtfState;
var
  AValue: Integer;
begin
  if TryStrToInt(FStrValue, AValue) then
    FValue := AValue;
end;

function TdxShapePropertyIntegerValueDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxShapePropertyIntegerValueDestination.Create(Importer);
  TdxShapePropertyIntegerValueDestination(Result).FValue := FValue;
end;

procedure TdxShapePropertyIntegerValueDestination.ProcessCharCore(AChar: Char);
begin
  FStrValue := FStrValue + AChar;
end;

{ TdxShapePropertyStringValueDestination }

function TdxShapePropertyStringValueDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxShapePropertyStringValueDestination.Create(Importer);
  TdxShapePropertyStringValueDestination(Result).FValue := Value;
end;

procedure TdxShapePropertyStringValueDestination.ProcessCharCore(AChar: Char);
begin
  FValue := FValue + AChar;
end;

{ TdxShapePropertyNameDestination }

function TdxShapePropertyNameDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxShapePropertyNameDestination.Create(Importer)
end;

end.
