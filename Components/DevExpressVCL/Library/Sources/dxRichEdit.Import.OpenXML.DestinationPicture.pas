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

unit dxRichEdit.Import.OpenXML.DestinationPicture;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGeometry,

  dxRichEdit.DocumentModel.IndexBasedObject,
  dxXMLReader,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.Import.CSSParser,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.Import.FloatingObject,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.OfficeImage;

type

  TdxInlinePictureCssParser = class;

  { TdxInlineObjectDestination }

  TdxInlineObjectDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
      FHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>;
      FVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    class function CreateHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>; static;
    class function CreateVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>; static;
  strict private
    function GetImage: TdxOfficeImageReference;
    procedure SetImage(const AValue: TdxOfficeImageReference);
    function GetFloatingObject: TdxFloatingObjectProperties;
  protected
    FFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
    FStyle: string;
    class procedure Add<T>(ATable: TDictionary<T, TdxWordProcessingMLValue>; AKey: T; const AOpenXmlValue: string); overload; static;
    class procedure Add<T>(ATable: TdxMLDictionary<T>; AKey: T; const AOpenXmlValue: string); overload; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function GetHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>; virtual;
    function GetVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>; virtual;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxInlineObjectDestination; static;
    class function OnShape(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    procedure AppendImage(APieceTable: TdxPieceTable; APosition: TdxImportInputPosition; AImage: TdxOfficeImageReference;
      AScaleX: Single; AScaleY: Single; AFillColor: TdxAlphaColor); virtual;
    function GetCssParser(const ASize: TSize): TdxInlinePictureCssParser;
    procedure ImportFloatingObject(ACssParser: TdxInlinePictureCssParser);
    function ShouldInsertPicture: Boolean;
    function CalculateScale(const AOriginalSize: TSize; ACssParser: TdxInlinePictureCssParser): TdxSizeF;
    function IsFloatingObject: Boolean;

    property HorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType> read GetHorizontalPositionTypeAttributeTable;
    property VerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType> read GetVerticalPositionTypeAttributeTable;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    destructor Destroy; override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    function ConvertToHorizontalPositionType(const AValue: string): TdxFloatingObjectHorizontalPositionType; virtual;
    function ConvertToVerticalPositionType(const AValue: string): TdxFloatingObjectVerticalPositionType; virtual;

    property Image: TdxOfficeImageReference read GetImage write SetImage;
    property FloatingObject: TdxFloatingObjectProperties read GetFloatingObject;
    property Style: string read FStyle write FStyle;
    property FloatingObjectImportInfo: TdxFloatingObjectImportInfo read FFloatingObjectImportInfo;
    class property OpenXmlHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType> read FHorizontalPositionTypeAttributeTable;
    class property OpenXmlVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType> read FVerticalPositionTypeAttributeTable;
  end;

  { TdxInlinePictureDestination }

  TdxInlinePictureDestination = class(TdxInlineObjectDestination);

  { TdxVMLShapeDestination }

  TdxVMLShapeDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FInlineObjectDestination: TdxInlineObjectDestination;
    FIsStroked: Boolean;
    FIsFilled: Boolean;
    FOutlineColor: TdxAlphaColor;
    FFillColor: TdxAlphaColor;
    FOutlineWidth: Single;
    function GetFloatingObject: TdxFloatingObjectProperties;
    function GetImage: TdxOfficeImageReference;
    procedure SetImage(const AValue: TdxOfficeImageReference);
    function GetFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
    function GetStyle: string;
  private
    function GetImporter: TdxWordProcessingMLBaseImporter; reintroduce; inline;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;

    property Importer: TdxWordProcessingMLBaseImporter read GetImporter;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AInlineObjectDestination: TdxInlineObjectDestination);
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxVMLShapeDestination; static;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    procedure ApplyShapeProperties(AShape: TdxShape);
    procedure ApplyTextBoxShapeProperties(AShape: TdxShape);
    procedure ApplyPictureShapeProperties(AShape: TdxShape);
    procedure ReadFloatingObjectProperties(AReader: TdxXmlReader); overload;
    procedure ReadShapeProperties(AReader: TdxXmlReader); overload;
    procedure ReadFloatingObjectProperties(AReader: TdxXmlReader; AProperties: TdxFloatingObjectProperties); overload;
    procedure ReadShapeProperties(AReader: TdxXmlReader; AShape: TdxShape); overload;
    function GetBoolValue(const AValue: string): Boolean; inline;
    class function OnImageData(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWrap(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLock(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTextBox(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAnchorLock(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property FloatingObject: TdxFloatingObjectProperties read GetFloatingObject;
    property Image: TdxOfficeImageReference read GetImage write SetImage;
    property FloatingObjectImportInfo: TdxFloatingObjectImportInfo read GetFloatingObjectImportInfo;
    property Style: string read GetStyle;
  end;

  { TdxAnchorLockDestination }

  TdxAnchorLockDestination = class(TdxLeafElementDestination)
  strict private
    FFloatingObjectProperties: TdxFloatingObjectProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxShapeLockDestination }

  TdxShapeLockDestination = class(TdxLeafElementDestination)
  strict private
    FFloatingObjectProperties: TdxFloatingObjectProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxLockAspectRatioTable }

  TdxLockAspectRatioTable = class(TdxStringsDictionary);

  { TdxVMLImageDataDestination }

  TdxVMLImageDataDestination = class(TdxLeafElementDestination)
  strict private
    FFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
    FStyle: string;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
      const AStyle: string = '');
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementOpenCore(const AId: string);

    property FloatingObjectImportInfo: TdxFloatingObjectImportInfo read FFloatingObjectImportInfo;
  end;

  { TdxWrapDestination }

  TdxWrapDestination = class(TdxLeafElementDestination)
  strict private
    FFloatingObject: TdxFloatingObjectProperties;
    procedure ImportTextWrapType(AReader: TdxXmlReader);
    procedure ImportTextWrapSide(AReader: TdxXmlReader);
  public
    constructor Create(AImporter: TdxWordProcessingMLBaseImporter; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxInlinePictureCssParser }

  TdxInlinePictureCssParser = class(TdxCustomCssParser)
  strict private
    FCssKeywordTable: TdxCssKeywordTranslatorTable;
    FOriginalSize: TSize;
    FTopDistance: Integer;
    FLeftDistance: Integer;
    FRightDistance: Integer;
    FBottomDistance: Integer;
    FHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    FVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    FTextBoxVerticalAlignment: TdxVerticalAlignment;
    FWrapText: Boolean;
    FUseWrapText: Boolean;
    FHorizontalPositionType: string;
    FVerticalPositionType: string;
    FOffset: TPoint;
    FSize: TSize;
    FZOrder: Integer;
    FRotation: Integer;
    FUseRotation: Boolean;
    FWidthPercent: Integer;
    FFromWidth: TdxFloatingObjectRelativeFromHorizontal;
    FUseRelativeWidth: Boolean;
    FHeightPercent: Integer;
    FFromHeight: TdxFloatingObjectRelativeFromVertical;
    FUseRelativeHeight: Boolean;
  protected
    function GetCssKeywordTable: TdxCssKeywordTranslatorTable; override;
    function CreateCssKeywordTable: TdxCssKeywordTranslatorTable;
    procedure CssWidth(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssHeight(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssVerticalPositionType(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssHorizontalPositionType(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    function GetFloatingObjectHorizontalPositionAlignment(const AValue: string): TdxFloatingObjectHorizontalPositionAlignment;
    function GetFloatingObjectVerticalPositionAlignment(const AValue: string): TdxFloatingObjectVerticalPositionAlignment;
    procedure CssHorizontalPositionAlignment(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssVerticalPositionAlignment(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssTextBoxVerticalAlignment(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssTextBoxWrapType(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    function GetWrapText(const AValue: string): Boolean;
    function GetFloatingObjectVerticalAlignment(const AValue: string): TdxVerticalAlignment;
    procedure CssZOrder(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    function GetModelUnitsFromUnitOrEmuValue(ACssUnitConverter: TdxDocumentModelUnitConverter; const APropertyValue: string): Integer;
    procedure CssTopDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssLeftDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssRightDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssBottomDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssMsoWidthPercent(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssMsoHeightPercent(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssMsoWidthRelative(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssMsoHeightRelative(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    function GetFloatingObjectRelativeFromHorizontal(const AValue: string): TdxFloatingObjectRelativeFromHorizontal;
    function GetFloatingObjectRelativeFromVertical(const AValue: string): TdxFloatingObjectRelativeFromVertical;
    function GetPercentValue(const AValue: string): Integer;
    procedure CssOffsetX(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssOffsetY(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
    procedure CssRotation(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
  public
    constructor Create(ADocumentModel: TdxDocumentModel; const AOriginalSize: TSize);
    destructor Destroy; override;

    property Size: TSize read FSize;
    property ZOrder: Integer read FZOrder;
    property TopDistance: Integer read FTopDistance;
    property LeftDistance: Integer read FLeftDistance;
    property RightDistance: Integer read FRightDistance;
    property BottomDistance: Integer read FBottomDistance;
    property Offset: TPoint read FOffset;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read FHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read FVerticalPositionAlignment;
    property TextBoxVerticalAlignment: TdxVerticalAlignment read FTextBoxVerticalAlignment;
    property HorizontalPositionType: string read FHorizontalPositionType;
    property VerticalPositionType: string read FVerticalPositionType;
    property WrapText: Boolean read FWrapText;
    property UseWrapText: Boolean read FUseWrapText;
    property Rotation: Integer read FRotation;
    property UseRotation: Boolean read FUseRotation;
    property WidthPercent: Integer read FWidthPercent;
    property FromWidth: TdxFloatingObjectRelativeFromHorizontal read FFromWidth;
    property UseRelativeWidth: Boolean read FUseRelativeWidth;
    property HeightPercent: Integer read FHeightPercent;
    property FromHeight: TdxFloatingObjectRelativeFromVertical read FFromHeight;
    property UseRelativeHeight: Boolean read FUseRelativeHeight;
  end;

implementation

uses
  Math, IOUtils, dxTypeHelpers,
  dxStringHelper,
  dxRichEdit.Utils.DXUnit,
  dxRichEdit.Utils.NumberParser,
  dxMeasurementUnits,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Import.OpenXML,
  dxRichEdit.Import.OpenXML.DestinationDrawing;

{ TdxInlineObjectDestination }

class constructor TdxInlineObjectDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
  FHorizontalPositionTypeAttributeTable := CreateHorizontalPositionTypeAttributeTable;
  FVerticalPositionTypeAttributeTable := CreateVerticalPositionTypeAttributeTable;
end;

class destructor TdxInlineObjectDestination.Finalize;
begin
  FHandlerTable.Free;
  FHorizontalPositionTypeAttributeTable.Free;
  FVerticalPositionTypeAttributeTable.Free;
end;

constructor TdxInlineObjectDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FFloatingObjectImportInfo := TdxFloatingObjectImportInfo.Create(AImporter.PieceTable);
end;

destructor TdxInlineObjectDestination.Destroy;
begin
  FreeAndNil(FFloatingObjectImportInfo);
  inherited Destroy;
end;

class function TdxInlineObjectDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('shape', OnShape);
end;

class function TdxInlineObjectDestination.CreateHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>.Create;
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.Column, 'text');
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.Margin, 'margin');
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.Page, 'page');
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.Character, 'char');
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.LeftMargin, 'left-margin-area');
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.RightMargin, 'right-margin-area');
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.InsideMargin, 'inner-margin-area');
  Add<TdxFloatingObjectHorizontalPositionType>(Result, TdxFloatingObjectHorizontalPositionType.OutsideMargin, 'outer-margin-area');
end;

class function TdxInlineObjectDestination.CreateVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectVerticalPositionType>.Create;
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.Margin, 'margin');
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.Page, 'page');
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.Line, 'line');
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.Paragraph, 'text');
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.TopMargin, 'top-margin-area');
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.BottomMargin, 'bottom-margin-area');
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.InsideMargin, 'inner-margin-area');
  Add<TdxFloatingObjectVerticalPositionType>(Result, TdxFloatingObjectVerticalPositionType.OutsideMargin, 'outer-margin-area');
end;

class procedure TdxInlineObjectDestination.Add<T>(ATable: TDictionary<T, TdxWordProcessingMLValue>; AKey: T; const AOpenXmlValue: string);
begin
  ATable.Add(AKey, TdxWordProcessingMLValue.Create(AOpenXmlValue));
end;

class procedure TdxInlineObjectDestination.Add<T>(ATable: TdxMLDictionary<T>; AKey: T; const AOpenXmlValue: string);
begin
  ATable.Add(AKey, TdxWordProcessingMLValue.Create(AOpenXmlValue));
end;

function TdxInlineObjectDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxInlineObjectDestination.GetImage: TdxOfficeImageReference;
begin
  Result := FloatingObjectImportInfo.Image;
end;

procedure TdxInlineObjectDestination.SetImage(const AValue: TdxOfficeImageReference);
begin
  FFloatingObjectImportInfo.Image := AValue;
end;

function TdxInlineObjectDestination.GetFloatingObject: TdxFloatingObjectProperties;
begin
  Result := FFloatingObjectImportInfo.FloatingObjectProperties;
end;

function TdxInlineObjectDestination.GetHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>;
begin
  Result := FHorizontalPositionTypeAttributeTable;
end;

function TdxInlineObjectDestination.GetVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>;
begin
  Result := FVerticalPositionTypeAttributeTable;
end;

class function TdxInlineObjectDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxInlineObjectDestination;
begin
  Result := TdxInlineObjectDestination(AImporter.PeekDestination);
end;

class function TdxInlineObjectDestination.OnShape(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxVMLShapeDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(TdxWordProcessingMLBaseImporter(AImporter)));
end;

procedure TdxInlineObjectDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  ACssParser: TdxInlinePictureCssParser;
  AOriginalSize: TSize;
  AScale: TdxSizeF;
  AFillColor: TdxAlphaColor;
begin
  if (FloatingObjectImportInfo.ShapeType = TdxShapeType.TextBox) and (FloatingObjectImportInfo.TextBoxContent <> nil) then
  begin
    FloatingObjectImportInfo.IsFloatingObject := True;
    ACssParser := GetCssParser(TSize.Create(0, 0));
    try
      ImportFloatingObject(ACssParser);
    finally
      ACssParser.Free;
    end;
  end
  else
  begin
    if Image = nil then
      Exit;
    if not ShouldInsertPicture then
    begin
      Importer.PieceTable.InsertTextCore(Importer.Position, ' ');
      Exit;
    end;

    AOriginalSize := Image.CalculateImageSizeInModelUnits(Importer.DocumentModel.UnitConverter);
    ACssParser := GetCssParser(AOriginalSize);
    try
      if IsFloatingObject then
      begin
        FloatingObjectImportInfo.IsFloatingObject := True;
        ImportFloatingObject(ACssParser)
      end
      else
      begin
        AScale := CalculateScale(AOriginalSize, ACssParser);
        if FloatingObjectImportInfo.Shape.UseFillColor then
          AFillColor := FloatingObjectImportInfo.Shape.FillColor
        else
          AFillColor := TdxAlphaColors.Empty;
        AppendImage(Importer.PieceTable, Importer.Position, Image, AScale.Width, AScale.Height, AFillColor);
        Exit;
      end;
    finally
      ACssParser.Free;
    end;
  end;
  FloatingObjectImportInfo.InsertFloatingObject(Importer.Position);
end;

procedure TdxInlineObjectDestination.AppendImage(APieceTable: TdxPieceTable; APosition: TdxImportInputPosition;
  AImage: TdxOfficeImageReference; AScaleX: Single; AScaleY: Single; AFillColor: TdxAlphaColor);
begin
  APieceTable.AppendImage(APosition, AImage, AScaleX, AScaleY, AFillColor, False);
end;

function TdxInlineObjectDestination.GetCssParser(const ASize: TSize): TdxInlinePictureCssParser;
var
  AReader: TStringReader;
begin
  Result := TdxInlinePictureCssParser.Create(Importer.DocumentModel, ASize);
  AReader := TStringReader.Create(FStyle);
  try
    Result.ParseAttribute(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxInlineObjectDestination.ImportFloatingObject(ACssParser: TdxInlinePictureCssParser);
begin
  FloatingObject.ZOrder := ACssParser.ZOrder;
  FloatingObject.ActualSize := TSize.Create(ACssParser.Size.Width, ACssParser.Size.Height);
  FloatingObject.TopDistance := ACssParser.TopDistance;
  FloatingObject.BottomDistance := ACssParser.BottomDistance;
  FloatingObject.LeftDistance := ACssParser.LeftDistance;
  FloatingObject.RightDistance := ACssParser.RightDistance;
  FloatingObject.Offset := ACssParser.Offset;

  if ACssParser.HorizontalPositionAlignment <> TdxFloatingObjectHorizontalPositionAlignment.None then
    FloatingObject.HorizontalPositionAlignment := ACssParser.HorizontalPositionAlignment;
  if ACssParser.VerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.None then
    FloatingObject.VerticalPositionAlignment := ACssParser.VerticalPositionAlignment;
  if ACssParser.UseRelativeWidth then
    FloatingObject.RelativeWidth := TdxFloatingObjectRelativeWidth.Create(ACssParser.FromWidth, ACssParser.WidthPercent);
  if ACssParser.UseRelativeHeight then
    FloatingObject.RelativeHeight := TdxFloatingObjectRelativeHeight.Create(ACssParser.FromHeight, ACssParser.HeightPercent);

  FloatingObject.HorizontalPositionType := ConvertToHorizontalPositionType(ACssParser.HorizontalPositionType);
  FloatingObject.VerticalPositionType := ConvertToVerticalPositionType(ACssParser.VerticalPositionType);

  if ACssParser.TextBoxVerticalAlignment <> TdxVerticalAlignment.Top then
    FloatingObjectImportInfo.TextBoxProperties.VerticalAlignment := ACssParser.TextBoxVerticalAlignment;
  if ACssParser.UseWrapText then
    FloatingObjectImportInfo.TextBoxProperties.WrapText := ACssParser.WrapText;

  if ACssParser.UseRotation then
    FloatingObjectImportInfo.Shape.Rotation := ACssParser.Rotation;
end;

function TdxInlineObjectDestination.ConvertToHorizontalPositionType(const AValue: string): TdxFloatingObjectHorizontalPositionType;
begin
  Result := HorizontalPositionTypeAttributeTable.GetKeyByOpenXmlStringDef(AValue, TdxFloatingObjectHorizontalPositionType.Column);
end;

function TdxInlineObjectDestination.ConvertToVerticalPositionType(const AValue: string): TdxFloatingObjectVerticalPositionType;
begin
  Result := VerticalPositionTypeAttributeTable.GetKeyByOpenXmlStringDef(AValue, TdxFloatingObjectVerticalPositionType.Paragraph);
end;

function TdxInlineObjectDestination.ShouldInsertPicture: Boolean;
begin
  Result := Importer.DocumentModel.DocumentCapabilities.InlinePicturesAllowed;
end;

function TdxInlineObjectDestination.CalculateScale(const AOriginalSize: TSize;
  ACssParser: TdxInlinePictureCssParser): TdxSizeF;
var
  AScaleX, AScaleY: Single;
begin
  if FStyle = '' then
    Exit(TdxSizeF.Create(100, 100));

  AScaleX := Max(1, MulDiv(100, ACssParser.Size.Width, AOriginalSize.Width));
  AScaleY := Max(1, MulDiv(100, ACssParser.Size.Height, AOriginalSize.Height));
  Result := TdxSizeF.Create(AScaleX, AScaleY);
end;

function TdxInlineObjectDestination.IsFloatingObject: Boolean;
begin
  Result := TdxStringHelper.Contains(Style, 'position:absolute');
end;

{ TdxVMLShapeDestination }

constructor TdxVMLShapeDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AInlineObjectDestination: TdxInlineObjectDestination);
begin
  inherited Create(AImporter);
  Assert(AInlineObjectDestination <> nil, 'inlineObjectDestination');
  FInlineObjectDestination := AInlineObjectDestination;
end;

class constructor TdxVMLShapeDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxVMLShapeDestination.Finalize;
begin
  FreeAndNil(FHandlerTable);
end;

function TdxVMLShapeDestination.GetImporter: TdxWordProcessingMLBaseImporter;
begin
  Result := TdxWordProcessingMLBaseImporter(inherited Importer);
end;

function TdxVMLShapeDestination.GetBoolValue(const AValue: string): Boolean;
begin
  Result := AValue = 't';
end;

class function TdxVMLShapeDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('imagedata', OnImageData);
  Result.Add('wrap', OnWrap);
  Result.Add('lock', OnLock);
  Result.Add('textbox', OnTextBox);
  Result.Add('anchorlock', OnAnchorLock);
end;

function TdxVMLShapeDestination.GetFloatingObject: TdxFloatingObjectProperties;
begin
  Result := FloatingObjectImportInfo.FloatingObjectProperties;
end;

function TdxVMLShapeDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxVMLShapeDestination.GetImage: TdxOfficeImageReference;
begin
  Result := FloatingObjectImportInfo.Image;
end;

procedure TdxVMLShapeDestination.SetImage(const AValue: TdxOfficeImageReference);
begin
  FloatingObjectImportInfo.Image := AValue;
end;

function TdxVMLShapeDestination.GetFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
begin
  Result := FInlineObjectDestination.FloatingObjectImportInfo;
end;

function TdxVMLShapeDestination.GetStyle: string;
begin
  Result := FInlineObjectDestination.Style;
end;

class function TdxVMLShapeDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxVMLShapeDestination;
begin
  Result := TdxVMLShapeDestination(AImporter.PeekDestination);
end;

procedure TdxVMLShapeDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AName: string;
begin
  FInlineObjectDestination.Style := AReader.GetAttribute('style');
  ReadFloatingObjectProperties(AReader);
  ReadShapeProperties(AReader);
  AName := AReader.GetAttribute('id');
  if (AName <> '') and (FloatingObjectImportInfo.Name = '') then
    FloatingObjectImportInfo.Name := AName;
end;

procedure TdxVMLShapeDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AShape: TdxShape;
begin
  inherited ProcessElementClose(AReader);

  AShape := FloatingObjectImportInfo.Shape;
  AShape.BeginUpdate;
  try
    ApplyShapeProperties(AShape);
  finally
    AShape.EndUpdate;
  end;
end;

procedure TdxVMLShapeDestination.ApplyShapeProperties(AShape: TdxShape);
begin
  if FloatingObjectImportInfo.ShapeType = TdxShapeType.TextBox then
    ApplyTextBoxShapeProperties(AShape)
  else
    ApplyPictureShapeProperties(AShape);
end;

procedure TdxVMLShapeDestination.ApplyTextBoxShapeProperties(AShape: TdxShape);
begin
  if FIsStroked then
  begin
    if FOutlineColor = TdxAlphaColors.Empty then
      FOutlineColor := TdxAlphaColors.Black;
    AShape.OutlineColor := FOutlineColor;
  end;

  if FIsFilled then
  begin
    if FFillColor <> TdxAlphaColors.Empty then
      AShape.FillColor := FFillColor;
  end;

  if FOutlineWidth = NullSingleValue then
    FOutlineWidth := 0.75;
  AShape.OutlineWidth := Round(UnitConverter.PointsToModelUnitsF(FOutlineWidth));
end;

procedure TdxVMLShapeDestination.ApplyPictureShapeProperties(AShape: TdxShape);
begin
  if FIsStroked then
  begin
    if FOutlineColor <> TdxAlphaColors.Empty then
      AShape.OutlineColor := FOutlineColor;
  end;
  if FIsFilled then
  begin
    if FFillColor <> TdxAlphaColors.Empty then
      AShape.FillColor := FFillColor;
  end;

  if FOutlineWidth <> NullSingleValue then
    AShape.OutlineWidth := Round(UnitConverter.PointsToModelUnitsF(FOutlineWidth));
end;

procedure TdxVMLShapeDestination.ReadFloatingObjectProperties(AReader: TdxXmlReader);
var
  AProperties: TdxFloatingObjectProperties;
begin
  AProperties := FloatingObjectImportInfo.FloatingObjectProperties;
  AProperties.BeginUpdate;
  try
    ReadFloatingObjectProperties(AReader, AProperties);
  finally
    AProperties.EndUpdate;
  end;
end;

procedure TdxVMLShapeDestination.ReadShapeProperties(AReader: TdxXmlReader);
var
  AShape: TdxShape;
begin
  AShape := FloatingObjectImportInfo.Shape;
  AShape.BeginUpdate;
  try
    ReadShapeProperties(AReader, AShape);
  finally
    AShape.EndUpdate;
  end;
end;

procedure TdxVMLShapeDestination.ReadFloatingObjectProperties(AReader: TdxXmlReader; AProperties: TdxFloatingObjectProperties);
var
  ATypeId, ALayoutInTableCell, AAllowOverlap: string;
  AUseLockAspectRatio, ALockAspectRatio: Boolean;
begin
  ATypeId := AReader.GetAttribute('type');
  if ATypeId <> '' then
  begin
    ALockAspectRatio := Importer.LockAspectRatioTableGetValue(ATypeId, AUseLockAspectRatio);
    if AUseLockAspectRatio then
      AProperties.LockAspectRatio := ALockAspectRatio;
  end;

  if AReader.GetAttribute('side') <> '' then
    AProperties.TextWrapSide := Importer.GetEnumValue<TdxFloatingObjectTextWrapSide>(AReader, 'wrapText',
    TdxOpenXmlExporter.FloatingObjectTextWrapSideTable, TdxFloatingObjectTextWrapSide.Both);

  ALayoutInTableCell := AReader.GetAttribute('allowincell', TdxOpenXmlExporter.OfficeNamespaceConst);
  if ALayoutInTableCell <> '' then
    AProperties.LayoutInTableCell := GetBoolValue(ALayoutInTableCell)
  else
    AProperties.LayoutInTableCell := True;

  AAllowOverlap := AReader.GetAttribute('allowoverlap', TdxOpenXmlExporter.OfficeNamespaceConst);
  if AAllowOverlap <> '' then
    AProperties.AllowOverlap := GetBoolValue(AAllowOverlap);
end;

procedure TdxVMLShapeDestination.ReadShapeProperties(AReader: TdxXmlReader; AShape: TdxShape);
var
  AStrokedAttribute, AFilledAttribute: string;
  AFillColor: TdxAlphaColor;
begin
  AStrokedAttribute := AReader.GetAttribute('stroked');
  FIsStroked := (AStrokedAttribute <> 'f') and (AStrokedAttribute <> 'false');
  AFilledAttribute := AReader.GetAttribute('filled');
  FIsFilled := (AFilledAttribute <> 'f') and (AFilledAttribute <> 'false');
  FOutlineColor := Importer.GetMSWordColorValue(AReader, 'strokecolor');
  AFillColor := Importer.GetMSWordColorValue(AReader, 'fillcolor');
  if AFillColor = TdxAlphaColors.Empty then
    FFillColor := TdxAlphaColors.White
  else
    FFillColor := AFillColor;
  FOutlineWidth := Importer.GetFloatValueInPoints(AReader, 'strokeweight', NullSingleValue);
end;

class function TdxVMLShapeDestination.OnImageData(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxVMLImageDataDestination.Create(TdxWordProcessingMLBaseImporter(AImporter),
    GetThis(TdxWordProcessingMLBaseImporter(AImporter)).FloatingObjectImportInfo,
    GetThis(TdxWordProcessingMLBaseImporter(AImporter)).Style);
end;

class function TdxVMLShapeDestination.OnWrap(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWrapDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), (GetThis(AImporter)).FloatingObject);
end;

class function TdxVMLShapeDestination.OnLock(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxShapeLockDestination.Create(TdxWordProcessingMLBaseImporter(AImporter),
    GetThis(TdxWordProcessingMLBaseImporter(AImporter)).FloatingObject);
end;

class function TdxVMLShapeDestination.OnTextBox(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  GetThis(AImporter).FloatingObjectImportInfo.ShapeType := TdxShapeType.TextBox;
  Result := TdxTextBoxDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo);
end;

class function TdxVMLShapeDestination.OnAnchorLock(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAnchorLockDestination.Create(TdxWordProcessingMLBaseImporter(AImporter),
    GetThis(TdxWordProcessingMLBaseImporter(AImporter)).FloatingObject);
end;

{ TdxAnchorLockDestination }

constructor TdxAnchorLockDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  inherited Create(AImporter);
  FFloatingObjectProperties := AFloatingObjectProperties;
end;

procedure TdxAnchorLockDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FFloatingObjectProperties.Locked := True;
end;

{ TdxShapeLockDestination }

constructor TdxShapeLockDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  inherited Create(AImporter);
  FFloatingObjectProperties := AFloatingObjectProperties;
end;

procedure TdxShapeLockDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AAspectRatioValue: string;
begin
  AAspectRatioValue := AReader.GetAttribute('aspectratio');
  if AAspectRatioValue <> '' then
    FFloatingObjectProperties.LockAspectRatio := (AAspectRatioValue = 't');
end;

{ TdxVMLImageDataDestination }

constructor TdxVMLImageDataDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo; const AStyle: string);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'floatingObjectImportInfo');
  FFloatingObjectImportInfo := AFloatingObjectImportInfo;
  FStyle := AStyle;
end;

procedure TdxVMLImageDataDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AId: string;
begin
  AId := AReader.GetAttribute('id', TdxOpenXmlExporter.RelsNamespace);
  if (AId = '') or (not Importer.DocumentModel.DocumentCapabilities.InlinePicturesAllowed) then
    Exit;
  ProcessElementOpenCore(AId);
end;

procedure TdxVMLImageDataDestination.ProcessElementOpenCore(const AId: string);
var
  AOpenXmlImporter: TdxOpenXmlImporter;
  AImage: TdxOfficeImageReference;
  AFileName: string;
  ACssParser: TdxInlinePictureCssParser;
  AStringReader: TStringReader;
begin
  AOpenXmlImporter := TdxOpenXmlImporter(Importer);
  AImage := nil;
  AFileName := AOpenXmlImporter.LookupRelationTargetById(AOpenXmlImporter.DocumentRelations, AId, AOpenXmlImporter.DocumentRootFolder, '');
  if TPath.GetExtension(AFileName) = '.wmf' then
  begin
    if FStyle <> '' then
    begin
      ACssParser := TdxInlinePictureCssParser.Create(Importer.DocumentModel, TSize.Create(100, 100));
      try
        AStringReader := TStringReader.Create(FStyle);
        try
          ACssParser.ParseAttribute(AStringReader);
        finally
          AStringReader.Free;
        end;
        AImage := AOpenXmlImporter.LookupMetafileByRelationId(AId, AOpenXmlImporter.DocumentRootFolder,
          ACssParser.Size.Width, ACssParser.Size.Height);
      finally
        ACssParser.Free;
      end;
    end;
  end
  else
    AImage := AOpenXmlImporter.LookupImageByRelationId(AId, AOpenXmlImporter.DocumentRootFolder);
  if AImage <> nil then
    FFloatingObjectImportInfo.Image := AImage;
end;

{ TdxWrapDestination }

constructor TdxWrapDestination.Create(AImporter: TdxWordProcessingMLBaseImporter; AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  inherited Create(AImporter);
  FFloatingObject := AFloatingObjectProperties;
end;

procedure TdxWrapDestination.ImportTextWrapType(AReader: TdxXmlReader);
var
  ATextWrapType: string;
  AKey: TdxFloatingObjectTextWrapType;
begin
  ATextWrapType := AReader.GetAttribute('type');
  if TdxWordProcessingMLBaseExporter.FloatingObjectTextWrapTypeTable.TryGetKeyByWordMLString(ATextWrapType, AKey) then
    FFloatingObject.TextWrapType := AKey;
end;

procedure TdxWrapDestination.ImportTextWrapSide(AReader: TdxXmlReader);
var
  ATextWrapSide: string;
  AKey: TdxFloatingObjectTextWrapSide;
begin
  ATextWrapSide := AReader.GetAttribute('side');
  if TdxWordProcessingMLBaseExporter.FloatingObjectTextWrapSideTable.TryGetKeyByWordMLString(ATextWrapSide, AKey) then
    FFloatingObject.TextWrapSide := AKey;
end;

procedure TdxWrapDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ImportTextWrapType(AReader);
  ImportTextWrapSide(AReader);
end;

{ TdxInlinePictureCssParser }

constructor TdxInlinePictureCssParser.Create(ADocumentModel: TdxDocumentModel; const AOriginalSize: TSize);
begin
  inherited Create(ADocumentModel);
  FOriginalSize := AOriginalSize;
  FSize := AOriginalSize;
  FCssKeywordTable := CreateCssKeywordTable;
  FLeftDistance := ADocumentModel.UnitConverter.TwipsToModelUnits(180);
  FRightDistance := ADocumentModel.UnitConverter.TwipsToModelUnits(180);
  FFromWidth := TdxFloatingObjectRelativeFromHorizontal.Page;
  FFromHeight := TdxFloatingObjectRelativeFromVertical.Page;
end;

function TdxInlinePictureCssParser.GetCssKeywordTable: TdxCssKeywordTranslatorTable;
begin
  Result := FCssKeywordTable;
end;

function TdxInlinePictureCssParser.CreateCssKeywordTable: TdxCssKeywordTranslatorTable;
begin
  Result := TdxCssKeywordTranslatorTable.Create;
  Result.Add(ConvertKeyToUpper('width'), CssWidth);
  Result.Add(ConvertKeyToUpper('height'), CssHeight);
  Result.Add(ConvertKeyToUpper('z-index'), CssZOrder);
  Result.Add(ConvertKeyToUpper('mso-wrap-distance-top'), CssTopDistance);
  Result.Add(ConvertKeyToUpper('mso-wrap-distance-left'), CssLeftDistance);
  Result.Add(ConvertKeyToUpper('mso-wrap-distance-right'), CssRightDistance);
  Result.Add(ConvertKeyToUpper('mso-wrap-distance-bottom'), CssBottomDistance);
  Result.Add(ConvertKeyToUpper('margin-left'), CssOffsetX);
  Result.Add(ConvertKeyToUpper('margin-top'), CssOffsetY);
  Result.Add(ConvertKeyToUpper('mso-position-horizontal'), CssHorizontalPositionAlignment);
  Result.Add(ConvertKeyToUpper('mso-position-vertical'), CssVerticalPositionAlignment);
  Result.Add(ConvertKeyToUpper('mso-position-vertical-relative'), CssVerticalPositionType);
  Result.Add(ConvertKeyToUpper('mso-position-horizontal-relative'), CssHorizontalPositionType);
  Result.Add(ConvertKeyToUpper('v-text-anchor'), CssTextBoxVerticalAlignment);
  Result.Add(ConvertKeyToUpper('mso-wrap-style'), CssTextBoxWrapType);
  Result.Add(ConvertKeyToUpper('rotation'), CssRotation);
  Result.Add(ConvertKeyToUpper('mso-width-percent'), CssMsoWidthPercent);
  Result.Add(ConvertKeyToUpper('mso-height-percent'), CssMsoHeightPercent);
  Result.Add(ConvertKeyToUpper('mso-width-relative'), CssMsoWidthRelative);
  Result.Add(ConvertKeyToUpper('mso-height-relative'), CssMsoHeightRelative);
end;

procedure TdxInlinePictureCssParser.CssWidth(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: TdxUnit;
  AParameters: TdxUnitConversionParameters;
  AConverter: TUnitConverter;
begin
  AValue := TdxUnit.Create(APropertiesValue[0]);
  try
    AParameters := TdxUnitConversionParameters.Empty;
    AParameters.OriginalValue := FOriginalSize.Width;
    AConverter := TUnitConverter.Create(ACssProperties.UnitConverter);
    FSize.Width := AConverter.ToModelUnits(AValue, AParameters);
  finally
    AValue.Free;
  end;
end;

procedure TdxInlinePictureCssParser.CssHeight(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: TdxUnit;
  AParameters: TdxUnitConversionParameters;
  AConverter: TUnitConverter;
begin
  AValue := TdxUnit.Create(APropertiesValue[0]);
  try
    AParameters := TdxUnitConversionParameters.Empty;
    AParameters.OriginalValue := FOriginalSize.Height;
    AConverter := TUnitConverter.Create(ACssProperties.UnitConverter);
    FSize.Height := AConverter.ToModelUnits(AValue, AParameters);
  finally
    AValue.Free;
  end;
end;

procedure TdxInlinePictureCssParser.CssVerticalPositionType(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FVerticalPositionType := APropertiesValue[0];
end;

procedure TdxInlinePictureCssParser.CssHorizontalPositionType(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FHorizontalPositionType := APropertiesValue[0];
end;

function TdxInlinePictureCssParser.GetFloatingObjectHorizontalPositionAlignment(const AValue: string): TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxWordProcessingMLBaseExporter.FloatingObjectHorizontalPositionAlignmentTable.GetKeyByStringDef(
    AValue, TdxFloatingObjectHorizontalPositionAlignment.None);
end;

function TdxInlinePictureCssParser.GetFloatingObjectVerticalPositionAlignment(const AValue: string): TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxWordProcessingMLBaseExporter.FloatingObjectVerticalPositionAlignmentTable.GetKeyByStringDef(
    AValue, TdxFloatingObjectVerticalPositionAlignment.None);
end;

procedure TdxInlinePictureCssParser.CssHorizontalPositionAlignment(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FHorizontalPositionAlignment := GetFloatingObjectHorizontalPositionAlignment(APropertiesValue[0]);
end;

procedure TdxInlinePictureCssParser.CssVerticalPositionAlignment(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FVerticalPositionAlignment := GetFloatingObjectVerticalPositionAlignment(APropertiesValue[0]);
end;

procedure TdxInlinePictureCssParser.CssTextBoxVerticalAlignment(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FTextBoxVerticalAlignment := GetFloatingObjectVerticalAlignment(APropertiesValue[0]);
end;

procedure TdxInlinePictureCssParser.CssTextBoxWrapType(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FWrapText := GetWrapText(APropertiesValue[0]);
  FUseWrapText := True;
end;

function TdxInlinePictureCssParser.GetWrapText(const AValue: string): Boolean;
begin
  Result := AValue = 'square';
end;

function TdxInlinePictureCssParser.GetFloatingObjectVerticalAlignment(const AValue: string): TdxVerticalAlignment;
begin
  Result := TdxWordProcessingMLBaseExporter.TextBoxVerticalAlignmentTable.GetKeyByStringDef(AValue, TdxVerticalAlignment.Top);
end;

procedure TdxInlinePictureCssParser.CssZOrder(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: TdxUnit;
begin
  AValue := TdxUnit.Create(APropertiesValue[0], MinInt, MaxInt);
  try
    FZOrder := Trunc(AValue.Value);
  finally
    AValue.Free;
  end;
end;

destructor TdxInlinePictureCssParser.Destroy;
begin
  FreeAndNil(FCssKeywordTable);
  inherited Destroy;
end;

function TdxInlinePictureCssParser.GetModelUnitsFromUnitOrEmuValue(ACssUnitConverter: TdxDocumentModelUnitConverter;
  const APropertyValue: string): Integer;
var
  AEmuValue: Integer;
  AConverter: TUnitConverter;
  AValue: TdxUnit;
begin
  AConverter := TUnitConverter.Create(ACssUnitConverter);
  if not TdxNumber.TryParse(APropertyValue, TdxNumberStyles.Integer, AEmuValue) then
  begin
    AValue := TdxUnit.Create(APropertyValue);
    try
      Result := AConverter.ToModelUnits(AValue);
    finally
      AValue.Free;
    end;
  end
  else
    Result := ACssUnitConverter.EmuToModelUnits(AEmuValue);
end;

procedure TdxInlinePictureCssParser.CssTopDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FTopDistance := GetModelUnitsFromUnitOrEmuValue(ACssProperties.UnitConverter, APropertiesValue[0]);
end;

procedure TdxInlinePictureCssParser.CssLeftDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FLeftDistance := GetModelUnitsFromUnitOrEmuValue(ACssProperties.UnitConverter, APropertiesValue[0]);
end;

procedure TdxInlinePictureCssParser.CssRightDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FRightDistance := GetModelUnitsFromUnitOrEmuValue(ACssProperties.UnitConverter, APropertiesValue[0]);
end;

procedure TdxInlinePictureCssParser.CssBottomDistance(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FBottomDistance := GetModelUnitsFromUnitOrEmuValue(ACssProperties.UnitConverter, APropertiesValue[0]);
end;

procedure TdxInlinePictureCssParser.CssMsoWidthPercent(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FWidthPercent := GetPercentValue(APropertiesValue[0]);
  FUseRelativeWidth := True;
end;

procedure TdxInlinePictureCssParser.CssMsoHeightPercent(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FHeightPercent := GetPercentValue(APropertiesValue[0]);
  FUseRelativeHeight := True;
end;

procedure TdxInlinePictureCssParser.CssMsoWidthRelative(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FFromWidth := GetFloatingObjectRelativeFromHorizontal(APropertiesValue[0]);
  FUseRelativeWidth := True;
end;

procedure TdxInlinePictureCssParser.CssMsoHeightRelative(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  FFromHeight := GetFloatingObjectRelativeFromVertical(APropertiesValue[0]);
  FUseRelativeHeight := True;
end;

function TdxInlinePictureCssParser.GetFloatingObjectRelativeFromHorizontal(const AValue: string): TdxFloatingObjectRelativeFromHorizontal;
var
  ATable: TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal>;
begin
  ATable := TdxWordProcessingMLBaseExporter.FloatingObjectCssRelativeFromHorizontalTable;
  Result := ATable.GetKeyByStringDef(AValue, TdxFloatingObjectRelativeFromHorizontal.Margin);
end;

function TdxInlinePictureCssParser.GetFloatingObjectRelativeFromVertical(const AValue: string): TdxFloatingObjectRelativeFromVertical;
var
  ATable: TdxMLDictionary<TdxFloatingObjectRelativeFromVertical>;
begin
  ATable := TdxWordProcessingMLBaseExporter.FloatingObjectCssRelativeFromVerticalTable;
  Result := ATable.GetKeyByStringDef(AValue, TdxFloatingObjectRelativeFromVertical.Margin);
end;

function TdxInlinePictureCssParser.GetPercentValue(const AValue: string): Integer;
var
  ATrimValue: string;
  AFloatResult: Double;
  ALength, AIntResult: Integer;
begin
  ATrimValue := Trim(AValue);
  ALength := Length(ATrimValue);
  if (ALength <> 0) and (ATrimValue[ALength] = '%') then
  begin
    if TdxNumber.TryParse(TdxStringHelper.Substring(ATrimValue, 0, ALength - 1), TdxNumberStyles.Float, AFloatResult) then
      Exit(Trunc(AFloatResult * 1000));
  end
  else
  begin
    if TdxNumber.TryParse(AValue, TdxNumberStyles.Integer, AIntResult) then
      Exit(AIntResult * 100);
  end;
  Result := 100 * 1000;
end;

procedure TdxInlinePictureCssParser.CssOffsetX(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: TdxUnit;
  AConverter: TUnitConverter;
begin
  AValue := TdxUnit.Create(APropertiesValue[0]);
  try
    AConverter := TUnitConverter.Create(ACssProperties.UnitConverter);
    FOffset.X := AConverter.ToModelUnits(AValue);
  finally
    AValue.Free;
  end;
end;

procedure TdxInlinePictureCssParser.CssOffsetY(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: TdxUnit;
  AConverter: TUnitConverter;
begin
  AValue := TdxUnit.Create(APropertiesValue[0]);
  try
    AConverter := TUnitConverter.Create(ACssProperties.UnitConverter);
    FOffset.Y := AConverter.ToModelUnits(AValue);
  finally
    AValue.Free;
  end;
end;

procedure TdxInlinePictureCssParser.CssRotation(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: TdxRotationUnit;
  AConverter: TUnitConverter;
begin
  FUseRotation := True;
  AValue := TdxRotationUnit.Create(APropertiesValue[0]);
  try
    AConverter := TUnitConverter.Create(ACssProperties.UnitConverter);
    if AValue.&Type = TdxUnitType.Deg then
      FRotation := AConverter.DegreeToModelUnits(AValue)
    else
      FRotation := AConverter.FDToModelUnits(AValue);
  finally
    AValue.Free;
  end;
end;


end.
