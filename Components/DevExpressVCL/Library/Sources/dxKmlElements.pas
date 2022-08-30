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

unit dxKmlElements;

interface

{$I cxVer.inc}

uses
  Classes, SysUtils, RTLConsts, StrUtils, Windows,
  dxCore, dxCoreClasses, dxColorPicker, dxCoreGraphics, dxXmlDoc, dxCustomTree;

type
  TdxKmlElement = class
  protected
    function DoCreateAndParseElement(ANode: TdxXMLNode): TdxKmlElement;
    procedure Parse(ANode: TdxXMLNode); virtual; abstract;
  public
    function CreateAndParseElement(ANode: TdxXMLNode; const AElementName: TdxXMLString): TdxKmlElement; overload;
    function CreateAndParseElement(ANode: TdxXMLNode; const AElementNames: array of TdxXMLString): TdxKmlElement; overload;
    function CreateAndParseElements(ANode: TdxXMLNode; const AElementName: TdxXMLString): TdxFastObjectList; overload;
    function CreateAndParseElements(ANode: TdxXMLNode; const AElementNames: array of TdxXMLString): TdxFastObjectList; overload;
    class function GetParseName: string; virtual;
    function ParseBooleanElement(ANode: TdxXMLNode; const AElementName: TdxXMLString; ADefaultValue: Boolean): Boolean;
    function ParseDoubleAttribute(ANode: TdxXMLNode; const AElementName: TdxXMLString; ADefaultValue: Double): Double;
    function ParseDoubleElement(ANode: TdxXMLNode; const AElementName: TdxXMLString; ADefaultValue: Double): Double;
    function ParseStringAttribute(ANode: TdxXMLNode;
      const AElementName: TdxXMLString; const ADefaultValue: string): string;
    function ParseStringElement(ANode: TdxXMLNode;
      const AElementName: TdxXMLString; const ADefaultValue: string): string;
  end;

  TdxKmlElementClass = class of TdxKmlElement;

  TdxKmlObject = class(TdxKmlElement)
  private
    FID: string;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    property ID: string read FID;
  end;

  TdxKmlStyleSelector = class(TdxKmlObject)
  end;

  TdxKmlFeature = class(TdxKmlObject)
  private
    FAddress: string;
    FDescription: string;
    FName: string;
    FPhoneNumber: string;
    FStyleSelector: TdxKmlStyleSelector;
    FStyleUri: string;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    destructor Destroy; override;

    property Address: string read FAddress;
    property Description: string read FDescription;
    property Name: string read FName;
    property PhoneNumber: string read FPhoneNumber;
    property StyleUrl: string read FStyleUri;
    property StyleSelector: TdxKmlStyleSelector read FStyleSelector;
  end;

  TdxKmlContainer = class(TdxKmlFeature)
  end;

  TdxKmlFolder = class(TdxKmlContainer)
  end;

  TdxKmlDocument = class(TdxKmlContainer)
  end;

  TdxKmlGeometry = class(TdxKmlObject)
  end;

  TdxKmlCoordinate = record
    Altitude, Latitude, Longitude: Double;
  end;

  TdxKmlCoordinates = array of TdxKmlCoordinate;

  TdxKmlCoordinatesGeometry = class(TdxKmlGeometry)
  private
    FCoordinates: TdxKmlCoordinates;
    function GetCoordinates(Index: Integer): TdxKmlCoordinate;
    function GetCoordinatesCount: Integer;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    destructor Destroy; override;
    property Coordinates [Index: Integer]: TdxKmlCoordinate read GetCoordinates;
    property CoordinatesCount: Integer read GetCoordinatesCount;
  end;

  TdxKmlPoint = class(TdxKmlCoordinatesGeometry)
  public
    class function GetParseName: string; override;
  end;

  TdxKmlLineString = class(TdxKmlCoordinatesGeometry)
  public
    class function GetParseName: string; override;
  end;

  TdxKmlLinearRing = class(TdxKmlCoordinatesGeometry)
  public
    class function GetParseName: string; override;
  end;

  TdxKmlLinearRingContainer = class(TdxKmlElement)
  private
    FLinearRing: TdxKmlLinearRing;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    destructor Destroy; override;
    property LinearRing: TdxKmlLinearRing read FLinearRing;
  end;

  TdxKmlOuterBoundaryIs = class(TdxKmlLinearRingContainer)
  public
    class function GetParseName: string; override;
  end;

  TdxKmlInnerBoundaryIs = class(TdxKmlLinearRingContainer)
  public
    class function GetParseName: string; override;
  end;

  TdxKmlPolygon = class(TdxKmlGeometry)
  private
    FOuterBoundaryIs: TdxKmlOuterBoundaryIs;
    FInnerBoundaryIs: TdxKmlInnerBoundaryIs;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    destructor Destroy; override;
    class function GetParseName: string; override;
    property OuterBoundaryIs: TdxKmlOuterBoundaryIs read FOuterBoundaryIs;
    property InnerBoundaryIs: TdxKmlInnerBoundaryIs read FInnerBoundaryIs;
  end;

  TdxKmlMultiGeometry = class(TdxKmlGeometry)
  private
    FGeometries: TdxFastObjectList;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    destructor Destroy; override;
    property Geometries: TdxFastObjectList read FGeometries;
  end;

  TdxKmlPlacemark = class(TdxKmlFeature)
  private
    FGeometry: TdxKmlGeometry;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    destructor Destroy; override;
    property Geometry: TdxKmlGeometry read FGeometry;
  end;

  TdxKmlIcon = class(TdxKmlElement)
  private
    FHRef: string;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    property HRef: string read FHRef;
  end;

  TdxKmlUnits = (kmluFraction, kmluPixels, kmluInsetPixels);

  TdxKmlHotSpot = class(TdxKmlElement)
  private
    FX, FY: Double;
    FXUnits, FYUnits: TdxKmlUnits;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
    function ParseUnit(ANode: TdxXMLNode; const AElementName: TdxXMLString;
      ADefaultValue: TdxKmlUnits): TdxKmlUnits;
  public
    class function GetParseName: string; override;
    property X: Double read FX;
    property XUnits: TdxKmlUnits read FXUnits;
    property Y: Double read FY;
    property YUnits: TdxKmlUnits read FYUnits;
  end;

  TdxKmlSubStyle = class(TdxKmlObject)
  protected
    function ParseColorElement(ANode: TdxXMLNode; const AElementName: TdxXMLString;
      ADefaultValue: TdxAlphaColor): TdxAlphaColor;
  end;

  TdxKmlColorMode = (kcmNormal, kcmRandom);

  TdxKmlColorStyle = class(TdxKmlSubStyle)
  private
    FColor: TdxAlphaColor;
    FColorMode: TdxKmlColorMode;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
    function ParseColorMode(ANode: TdxXMLNode; const AElementName: TdxXMLString;
      ADefaultValue: TdxKmlColorMode): TdxKmlColorMode;
  public
    property Color: TdxAlphaColor read FColor;
    property ColorMode: TdxKmlColorMode read FColorMode;
  end;

  TdxKmlIconStyle = class(TdxKmlColorStyle)
  private
    FScale: Double;
    FHeading: Double;
    FIcon: TdxKmlIcon;
    FHotSpot: TdxKmlHotSpot;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    destructor Destroy; override;
    class function GetParseName: string; override;
    property Heading: Double read FHeading;
    property HotSpot: TdxKmlHotSpot read FHotSpot;
    property Icon: TdxKmlIcon read FIcon;
    property Scale: Double read FScale;
  end;

  TdxKmlLabelStyle = class(TdxKmlColorStyle)
  private
    FScale: Double;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    property Scale: Double read FScale;
  end;

  TdxKmlLineStyle = class(TdxKmlColorStyle)
  private
    FWidth: Double;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    property Width: Double read FWidth;
  end;

  TdxKmlPolyStyle = class(TdxKmlColorStyle)
  private
    FFill: Boolean;
    FOutline: Boolean;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    property Fill: Boolean read FFill;
    property Outline: Boolean read FOutline;
  end;

  TdxKmlDisplayMode = (kdmDefault, kdmHide);

  TdxKmlBalloonStyle = class(TdxKmlSubStyle)
  private
    FBgColor: TdxAlphaColor;
    FDisplayMode: TdxKmlDisplayMode;
    FTextColor: TdxAlphaColor;
    FText: string;
    function ParseDisplayMode(ANode: TdxXMLNode; const AElementName: TdxXMLString;
      ADefaultValue: TdxKmlDisplayMode): TdxKmlDisplayMode;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    property BgColor: TdxAlphaColor read FBgColor;
    property DisplayMode: TdxKmlDisplayMode read FDisplayMode;
    property TextColor: TdxAlphaColor read FTextColor;
    property Text: string read FText;
  end;

  TdxKmlListItemType = (kliCheck, kliCheckOffOnly, kliCheckHideChildren, kliRadioFolder);
  TdxKmlItemIconMode = (kiimOpen, kiimClosed, kiimError, kiimFetching0, kiimFetching1, kiimFetching2);

  TdxKmlItemIcon = class(TdxKmlObject)
  private
    FItemIconMode: TdxKmlItemIconMode;
    FHref: string;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
    function ParseIconItemMode(ANode: TdxXMLNode; const AElementName: TdxXMLString;
      ADefaultValue: TdxKmlItemIconMode): TdxKmlItemIconMode;
  public
    class function GetParseName: string; override;
    property ItemIconMode: TdxKmlItemIconMode read FItemIconMode;
    property Href: string read FHref;
  end;

  TdxKmlListStyle = class(TdxKmlSubStyle)
  private
    FBgColor: TdxAlphaColor;
    FItemIconList: TdxFastObjectList;
    FListItemType: TdxKmlListItemType;
    function ParseListItemType(ANode: TdxXMLNode; const AElementName: TdxXMLString;
      ADefaultValue: TdxKmlListItemType): TdxKmlListItemType;
    procedure CreateAndParseItemIcons(ANode: TdxXMLNode);
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetParseName: string; override;
    property ListItemType: TdxKmlListItemType read FListItemType;
  end;

  TdxKmlStyle = class(TdxKmlStyleSelector)
  private
    FIconStyle: TdxKmlIconStyle;
    FLabelStyle: TdxKmlLabelStyle;
    FLineStyle: TdxKmlLineStyle;
    FPolyStyle: TdxKmlPolyStyle;
    FBalloonStyle: TdxKmlBalloonStyle;
    FListStyle: TdxKmlListStyle;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    class function GetParseName: string; override;
    destructor Destroy; override;
    property IconStyle: TdxKmlIconStyle read FIconStyle;
    property LabelStyle: TdxKmlLabelStyle read FLabelStyle;
    property LineStyle: TdxKmlLineStyle read FLineStyle;
    property PolyStyle: TdxKmlPolyStyle read FPolyStyle;
    property BalloonStyle: TdxKmlBalloonStyle read FBalloonStyle;
    property ListStyle: TdxKmlListStyle read FListStyle;
  end;

  TdxKmlStyleState = (kssNormal, kssHighlight);

  TdxKmlPair = class(TdxKmlObject)
  private
    FKey: TdxKmlStyleState;
    FStyle: TdxKmlStyle;
    FStyleUrl: string;
    function ParseKey(ANode: TdxXMLNode; const AElementName: TdxXMLString;
      ADefaultValue: TdxKmlStyleState): TdxKmlStyleState;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    destructor Destroy; override;
    class function GetParseName: string; override;
    property Key: TdxKmlStyleState read FKey;
    property Style: TdxKmlStyle read FStyle;
    property StyleUrl: string read FStyleUrl;
  end;

  TdxKmlStyleMap = class(TdxKmlStyleSelector)
  private
    FPairs: TdxFastObjectList;
  protected
    procedure Parse(ANode: TdxXMLNode); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetParseName: string; override;
    property Pairs: TdxFastObjectList read FPairs;
  end;

  TdxKmlRoot = class(TdxKmlElement)
  private
    FStyles: TdxFastObjectList;
    FStyleMaps: TdxFastObjectList;
    FPlacemarks: TdxFastObjectList;
  protected
    procedure Parse(ANode: TdxXMLNode); overload; override;
  public
    constructor Create;
    destructor Destroy; override;

    function GetStyleIDFromUrl(const AUrl: string): string;
    procedure ResolveElementStyles(APlacemark: TdxKmlFeature;
      out ANormalStyle, AHighlightedStyle: TdxKmlStyle);
    procedure ResolveStyleByUrl(const AUrl: string; out AStyle: TdxKmlStyle);
    procedure ResolveStylesByUrl(const AUrl: string;
      out ANormalStyle, AHighlightedStyle: TdxKmlStyle);
    procedure ResolveStyleMapStyles(AStyleMap: TdxKmlStyleMap;
      out ANormalStyle, AHighlightedStyle: TdxKmlStyle);

    procedure Parse(ADocument: TdxXMLDocument); reintroduce; overload;
    procedure Parse(AFileStream: TStream); reintroduce; overload;

    property Styles: TdxFastObjectList read FStyles;
    property StyleMaps: TdxFastObjectList read FStyleMaps;
    property Placemarks: TdxFastObjectList read FPlacemarks;
  end;

implementation

uses
  dxKmlTokens, cxGraphics;

type
  TdxKmlElementFactoryItem = class
  private
    FElementName: string;
    FElementType: TdxKmlElementClass;
  public
    constructor Create(const AName: string; AType: TdxKmlElementClass);
    property ElementName: string read FElementName;
    property ElementType: TdxKmlElementClass read FElementType;
  end;

  TdxKmlElementsFactory = class
  private
    FItems: TdxFastObjectList;
  protected
    function GetElementClassByName(const AName: string): TdxKmlElementClass;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterElement(const AName: string; AType: TdxKmlElementClass);
    procedure RegisterElements(AElementClasses: array of TdxKmlElementClass);
    function CreateElementByName(const AName: TdxXMLString): TdxKmlElement;
  end;

{ TdxKmlElementFactoryItem }

constructor TdxKmlElementFactoryItem.Create(const AName: string;
  AType: TdxKmlElementClass);
begin
  inherited Create;
  FElementName := AName;
  FElementType := AType;
end;

{ TdxKmlElementsFactory }

constructor TdxKmlElementsFactory.Create;
begin
  inherited;
  FItems := TdxFastObjectList.Create;
end;

destructor TdxKmlElementsFactory.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TdxKmlElementsFactory.GetElementClassByName(const AName: string): TdxKmlElementClass;
var
  I: Integer;
  AItem: TdxKmlElementFactoryItem;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    AItem := FItems[I] as TdxKmlElementFactoryItem;
    if AItem.ElementName = AName then
    begin
      Result := AItem.ElementType;
      Break;
    end;
  end;
end;

function TdxKmlElementsFactory.CreateElementByName(
  const AName: TdxXMLString): TdxKmlElement;
begin
  Result := GetElementClassByName(dxAnsiStringToString(AName)).Create;
end;

procedure TdxKmlElementsFactory.RegisterElement(const AName: string;
  AType: TdxKmlElementClass);
begin
  FItems.Add(TdxKmlElementFactoryItem.Create(AName, AType));
end;

procedure TdxKmlElementsFactory.RegisterElements(AElementClasses: array of TdxKmlElementClass);
var
  I: Integer;
begin
  for I := Low(AElementClasses) to High(AElementClasses) do
    RegisterElement(AElementClasses[I].GetParseName, AElementClasses[I]);
end;

var
  FKmlElementsFactory: TdxKmlElementsFactory;

function dxKmlElementsFactory: TdxKmlElementsFactory;
begin
  if FKmlElementsFactory = nil then
    FKmlElementsFactory := TdxKmlElementsFactory.Create;
  Result := FKmlElementsFactory;
end;

  { TdxKmlElement }

function TdxKmlElement.DoCreateAndParseElement(ANode: TdxXMLNode): TdxKmlElement;
begin
  Result := dxKmlElementsFactory.CreateElementByName(ANode.Name);
  Result.Parse(ANode);
end;

function TdxKmlElement.CreateAndParseElement(ANode: TdxXMLNode;
  const AElementName: TdxXMLString): TdxKmlElement;
var
  AChildNode: TdxXMLNode;
begin
  Result := nil;
  if ANode.HasChildren then
    AChildNode := ANode.FindChild(AElementName)
  else
    AChildNode := nil;
  if AChildNode <> nil then
    Result := DoCreateAndParseElement(AChildNode);
end;

function TdxKmlElement.CreateAndParseElement(ANode: TdxXMLNode;
  const AElementNames: array of TdxXMLString): TdxKmlElement;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(AElementNames) do
  begin
    Result := CreateAndParseElement(ANode, AElementNames[I]);
    if Result <> nil then
      Break;
  end;
end;

function TdxKmlElement.CreateAndParseElements(ANode: TdxXMLNode; const AElementName: TdxXMLString): TdxFastObjectList;
var
  AList: TdxFastObjectList;
begin
  AList := TdxFastObjectList.Create;
  ANode.ForEach(
    procedure (ANode: TdxXMLNode; AUserData: Pointer)
    begin
      if ANode.Name = AElementName then
        AList.Add(DoCreateAndParseElement(ANode));
    end);
  Result := AList;
end;

function TdxKmlElement.CreateAndParseElements(ANode: TdxXMLNode; const AElementNames: array of TdxXMLString): TdxFastObjectList;
var
  I: Integer;
begin
  Result := TdxFastObjectList.Create;
  ANode := ANode.First;
  while ANode <> nil do
  begin
    for I := Low(AElementNames) to High(AElementNames) do
      if ANode.Name = AElementNames[I] then
      begin
        Result.Add(DoCreateAndParseElement(ANode));
        Break;
      end;

    ANode := ANode.Next;
  end;
end;

class function TdxKmlElement.GetParseName: string;
begin
  Result := ''
end;

function TdxKmlElement.ParseBooleanElement(ANode: TdxXMLNode; const AElementName: TdxXMLString;
  ADefaultValue: Boolean): Boolean;
var
  AText: string;
begin
  AText := ParseStringElement(ANode, AElementName, '');
  Result := StrToBoolDef(AText, ADefaultValue);
end;

function TdxKmlElement.ParseDoubleAttribute(ANode: TdxXMLNode; const AElementName: TdxXMLString;
  ADefaultValue: Double): Double;
var
  AText: string;
begin
  AText := ParseStringAttribute(ANode, AElementName, '');
  Result := dxStrToFloat(AText);
end;

function TdxKmlElement.ParseDoubleElement(ANode: TdxXMLNode; const AElementName: TdxXMLString;
  ADefaultValue: Double): Double;
var
  AText: string;
begin
  AText := ParseStringElement(ANode, AElementName, '');
  Result := dxStrToFloatDef(AText, '.', ADefaultValue);
end;

function TdxKmlElement.ParseStringAttribute(ANode: TdxXMLNode;
  const AElementName: TdxXMLString;
  const ADefaultValue: string): string;
var
  AAttribute: TdxXMLNodeAttribute;
begin
  if ANode.Attributes.Find(AElementName, AAttribute) then
    Result := AAttribute.ValueAsString
  else
    Result := ADefaultValue;
end;

function TdxKmlElement.ParseStringElement(ANode: TdxXMLNode;
  const AElementName: TdxXMLString; const ADefaultValue: string): string;
var
  AChildNode: TdxXMLNode;
begin
  AChildNode := nil;
  if ANode.HasChildren then
    AChildNode := ANode.FindChild(AElementName);
  if AChildNode <> nil then
    Result := AChildNode.TextAsString
  else
    Result := ADefaultValue;
end;

{ TdxKmlObject }

procedure TdxKmlObject.Parse(ANode: TdxXMLNode);
begin
  FID := ParseStringAttribute(ANode, SdxKmlId, '');
end;

{ TdxKmlFeature }

destructor TdxKmlFeature.Destroy;
begin
  FreeAndNil(FStyleSelector);
  inherited;
end;

procedure TdxKmlFeature.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FName := ParseStringElement(ANode, SdxKmlName, '');
  FAddress := ParseStringElement(ANode, SdxKmlAddress, '');
  FPhoneNumber := ParseStringElement(ANode, sdxKmlPhoneNumber, '');
  FDescription := ParseStringElement(ANode, SdxKmlDescription, '');
  FStyleUri := ParseStringElement(ANode, SdxKmlStyleUrl, '');
  FStyleSelector := CreateAndParseElement(ANode, SdxKmlStyle) as TdxKmlStyle;
  if StyleSelector = nil then
    FStyleSelector := CreateAndParseElement(ANode, SdxKmlStyleMap) as TdxKmlStyleMap;
end;

{ TdxKmlCoordinatesGeometry }

destructor TdxKmlCoordinatesGeometry.Destroy;
begin
  inherited;
end;

procedure TdxKmlCoordinatesGeometry.Parse(ANode: TdxXMLNode);

  function IsEndOfTuples(const ARestOfTuples: string; out AEndOfTuple: Integer): Boolean;
  begin
    AEndOfTuple := Pos(' ', ARestOfTuples) - 1;
    Result := AEndOfTuple = -1;
  end;

  procedure ParseTuple(const ACurrentTuple: string);
  var
    ACommaPos1, ACommaPos2: Integer;
  begin
    SetLength(FCoordinates, CoordinatesCount + 1);
    ZeroMemory(@FCoordinates[CoordinatesCount - 1], SizeOf(TdxKmlCoordinate));
    ACommaPos1 := Pos(',', ACurrentTuple);
    if ACommaPos1 <> 0 then
      FCoordinates[CoordinatesCount - 1].Longitude := dxStrToFloat(Copy(ACurrentTuple, 1, ACommaPos1 - 1));
    ACommaPos2 := PosEx(',', ACurrentTuple, ACommaPos1 + 1);
    if ACommaPos2 = 0 then
      ACommaPos2 := Length(ACurrentTuple) + 1;
    FCoordinates[CoordinatesCount - 1].Latitude := dxStrToFloat(Copy(ACurrentTuple, ACommaPos1 + 1, ACommaPos2 - ACommaPos1 - 1));
  end;

var
  AEndOfTuple: Integer;
  ARestTuples: string;
  ACurrentTuple: string;
  ACoordinates: string;
begin
  inherited;
  ACoordinates := ParseStringElement(ANode, SdxKmlCoordinates, '');
  ARestTuples := Trim(ACoordinates);
  if Length(ACoordinates) > 0 then
  begin
    ARestTuples := ARestTuples + ' ';
    while not IsEndOfTuples(ARestTuples, AEndOfTuple) do
    begin
      ACurrentTuple := Copy(ARestTuples, 1, AEndOfTuple);
      Delete(ARestTuples, 1, AEndOfTuple);
      ARestTuples := TrimLeft(ARestTuples);
      ParseTuple(ACurrentTuple);
    end;
  end;
end;

function TdxKmlCoordinatesGeometry.GetCoordinates(
  Index: Integer): TdxKmlCoordinate;
begin
  Result := FCoordinates[Index];
end;

function TdxKmlCoordinatesGeometry.GetCoordinatesCount: Integer;
begin
  Result := Length(FCoordinates);
end;

{ TdxKmlPoint }

class function TdxKmlPoint.GetParseName: string;
begin
  Result := SdxKmlPoint;
end;

{ TdxKmlLineString }

class function TdxKmlLineString.GetParseName: string;
begin
  Result := SdxKmlLineString;
end;

{ TdxKmlLinearRing }

class function TdxKmlLinearRing.GetParseName: string;
begin
  Result := SdxKmlLinearRing;
end;

{ TdxKmlLinearRingContainer }

destructor TdxKmlLinearRingContainer.Destroy;
begin
  FreeAndNil(FLinearRing);
  inherited;
end;

procedure TdxKmlLinearRingContainer.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FLinearRing := CreateAndParseElement(ANode, SdxKmlLinearRing) as TdxKmlLinearRing;
end;

{ TdxKmlOuterBoundaryIs }

class function TdxKmlOuterBoundaryIs.GetParseName: string;
begin
  Result := SdxKmlOuterBoundaryIs;
end;

{ TdxKmlInnerBoundaryIs }

class function TdxKmlInnerBoundaryIs.GetParseName: string;
begin
  Result := SdxKmlInnerBoundaryIs;
end;

{ TdxKmlPolygon }

destructor TdxKmlPolygon.Destroy;
begin
  FreeAndNil(FOuterBoundaryIs);
  FreeAndNil(FInnerBoundaryIs);
  inherited Destroy;
end;

class function TdxKmlPolygon.GetParseName: string;
begin
  Result := SdxKmlPolygon;
end;

procedure TdxKmlPolygon.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FInnerBoundaryIs := CreateAndParseElement(ANode, SdxKmlInnerBoundaryIs) as TdxKmlInnerBoundaryIs;
  FOuterBoundaryIs := CreateAndParseElement(ANode, SdxKmlOuterBoundaryIs) as TdxKmlOuterBoundaryIs;
end;

{ TdxKmlMultiGeometry }

destructor TdxKmlMultiGeometry.Destroy;
begin
  FreeAndNil(FGeometries);
  inherited;
end;

class function TdxKmlMultiGeometry.GetParseName: string;
begin
  Result := SdxKmlMultiGeometry;
end;

procedure TdxKmlMultiGeometry.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FGeometries := CreateAndParseElements(ANode, [SdxKmlPoint, SdxKmlLineString, SdxKmlPolygon, SdxKmlMultiGeometry]);
end;

{ TdxKmlPlacemark }

destructor TdxKmlPlacemark.Destroy;
begin
  FreeAndNil(FGeometry);
  inherited;
end;

class function TdxKmlPlacemark.GetParseName: string;
begin
  Result := SdxKmlPlacemark;
end;

procedure TdxKmlPlacemark.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FGeometry := CreateAndParseElement(ANode, [SdxKmlLineString, SdxKmlPoint, SdxKmlPolygon,
    SdxKmlMultiGeometry]) as TdxKmlGeometry;
end;

{ TdxKmlIcon }

class function TdxKmlIcon.GetParseName: string;
begin
  Result := SdxKmlIcon;
end;

procedure TdxKmlIcon.Parse(ANode: TdxXMLNode);
begin
  FHRef := ParseStringElement(ANode, SdxKmlHRef, '');
end;

{ TdxKmlHotSpot }

class function TdxKmlHotSpot.GetParseName: string;
begin
  Result := SdxKmlHotSpot;
end;

procedure TdxKmlHotSpot.Parse(ANode: TdxXMLNode);
const
  DefaultX: Double = 0.5;
  DefaultY: Double = 0.5;
  DefaultUnit: TdxKmlUnits = kmluFraction;
begin
  FX := ParseDoubleAttribute(ANode, SdxKmlX, DefaultX);
  FY := ParseDoubleAttribute(ANode, SdxKmlY, DefaultY);
  FXUnits := ParseUnit(ANode, SdxKmlXUnits, DefaultUnit);
  FYUnits := ParseUnit(ANode, SdxKmlYUnits, DefaultUnit);
end;

function TdxKmlHotSpot.ParseUnit(ANode: TdxXMLNode; const AElementName: TdxXMLString;
  ADefaultValue: TdxKmlUnits): TdxKmlUnits;
var
  AText: string;
begin
  AText := ParseStringAttribute(ANode, AElementName, '');
  if AText = SdxKmlPixels then
    Result := kmluPixels
  else
    if AText = SdxKmlInsetPixels then
      Result := kmluInsetPixels
    else
      if AText = SdxKmlFraction then
        Result := kmluFraction
      else
        Result := ADefaultValue;
end;

{ TdxKmlSubStyle }

function TdxKmlSubStyle.ParseColorElement(ANode: TdxXMLNode;
  const AElementName: TdxXMLString; ADefaultValue: TdxAlphaColor): TdxAlphaColor;
var
  AColorHexStr: string;
begin
  AColorHexStr := ParseStringElement(ANode, AElementName, '');
  if AColorHexStr <> '' then
    Result := TdxColorHelper.HexCodeToAlphaColor(AColorHexStr, True)
  else
    Result := ADefaultValue;
end;

{ TdxKmlColorStyle }

procedure TdxKmlColorStyle.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FColor := ParseColorElement(ANode, SdxKmlColor, $FFFFFFFF);
  FColorMode := ParseColorMode(ANode, SdxKmlColorMode, kcmNormal);
end;

function TdxKmlColorStyle.ParseColorMode(ANode: TdxXMLNode;
  const AElementName: TdxXMLString; ADefaultValue: TdxKmlColorMode): TdxKmlColorMode;
var
  AText: string;
begin
  AText := ParseStringElement(ANode, AElementName, '');
  if AText = SdxKmlRandom then
    Result := kcmRandom
  else
    if AText = SdxKmlNormal then
      Result := kcmNormal
    else
      Result := ADefaultValue;
end;

{ TdxKmlIconStyle }

destructor TdxKmlIconStyle.Destroy;
begin
  FreeAndNil(FHotSpot);
  FreeAndNil(FIcon);
  inherited;
end;

class function TdxKmlIconStyle.GetParseName: string;
begin
  Result := SdxKmlIconStyle;
end;

procedure TdxKmlIconStyle.Parse(ANode: TdxXMLNode);
const
  DefaultScale: Double = 1.0;
  DefaultHeading: Double = 0;
begin
  inherited;
  FScale := ParseDoubleElement(ANode, SdxKmlScale, DefaultScale);
  FHeading := ParseDoubleElement(ANode, SdxKmlHeading, DefaultHeading);
  FIcon := CreateAndParseElement(ANode, SdxKmlIcon) as TdxKmlIcon;
  FHotSpot := CreateAndParseElement(ANode, SdxKmlHotSpot) as TdxKmlHotSpot;
end;

{ TdxKmlLabelStyle }

class function TdxKmlLabelStyle.GetParseName: string;
begin
  Result := SdxKmlLabelStyle;
end;

procedure TdxKmlLabelStyle.Parse(ANode: TdxXMLNode);
const
  DefaultScale: Double = 1.0;
begin
  inherited;
  FScale := ParseDoubleElement(ANode, SdxKmlScale, DefaultScale);
end;

{ TdxKmlLineStyle }

class function TdxKmlLineStyle.GetParseName: string;
begin
  Result := SdxKmlLineStyle;
end;

procedure TdxKmlLineStyle.Parse(ANode: TdxXMLNode);
const
  DefaultWidth: Double = 1.0;
begin
  inherited;
  FWidth := ParseDoubleElement(ANode, SdxKmlWidth, DefaultWidth);
end;

{ TdxKmlPolyStyle }

class function TdxKmlPolyStyle.GetParseName: string;
begin
  Result := SdxKmlPolyStyle;
end;

procedure TdxKmlPolyStyle.Parse(ANode: TdxXMLNode);
const
  DefaultFill: Boolean = True;
  DefaultOutline: Boolean = True;
begin
  inherited;
  FFill := ParseBooleanElement(ANode, SdxKmlFill, DefaultFill);
  FOutline := ParseBooleanElement(ANode, SdxKmlOutline, DefaultOutline);
end;

{ TdxKmlBalloonStyle }

class function TdxKmlBalloonStyle.GetParseName: string;
begin
  Result := SdxKmlBalloonStyle;
end;

procedure TdxKmlBalloonStyle.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FBgColor := ParseColorElement(ANode, SdxKmlBgColor, $FFFFFFFF);
  FDisplayMode := ParseDisplayMode(ANode, SdxKmlDisplayMode, kdmDefault);
  FTextColor := ParseColorElement(ANode, SdxKmlTextColor, $FF000000);
  FText := ParseStringElement(ANode, SdxKmlText, '');
end;

function TdxKmlBalloonStyle.ParseDisplayMode(ANode: TdxXMLNode;
  const AElementName: TdxXMLString;
  ADefaultValue: TdxKmlDisplayMode): TdxKmlDisplayMode;
var
  AText: string;
begin
  AText := ParseStringElement(ANode, AElementName, '');
  if AText = 'hide' then
    Result := kdmHide
  else
    if AText = 'default' then
      Result := kdmDefault
    else
      Result := ADefaultValue;
end;

{ TdxKmlItemIcon }

class function TdxKmlItemIcon.GetParseName: string;
begin
  Result := SdxKmlItemIcon;
end;

procedure TdxKmlItemIcon.Parse(ANode: TdxXMLNode);
const
  DefaultIconItemMode = kiimOpen;
begin
  inherited;
  FItemIconMode := ParseIconItemMode(ANode, SdxKmlIconItemMode, DefaultIconItemMode);
  FHref := ParseStringElement(ANode, SdxKmlHRef, '');
end;

function TdxKmlItemIcon.ParseIconItemMode(ANode: TdxXMLNode;
  const AElementName: TdxXMLString;
  ADefaultValue: TdxKmlItemIconMode): TdxKmlItemIconMode;
var
  AText: string;
begin
  AText := ParseStringElement(ANode, AElementName, '');
  if AText = SdxKmlOpen then
    Result := kiimOpen
  else
    if AText = SdxKmlClosed then
      Result := kiimClosed
    else
      if AText = SdxKmlError then
        Result := kiimError
      else
        if AText = SdxKmlFetching0 then
          Result := kiimFetching0
        else
          if AText = SdxKmlFetching1 then
            Result := kiimFetching1
          else
            if AText = SdxKmlFetching2 then
              Result := kiimFetching2
            else
              Result := ADefaultValue;
end;

{ TdxKmlListStyle }

constructor TdxKmlListStyle.Create;
begin
  inherited;
end;

destructor TdxKmlListStyle.Destroy;
begin
  FreeAndNil(FItemIconList);
  inherited;
end;

class function TdxKmlListStyle.GetParseName: string;
begin
  Result := SdxKmlListStyle;
end;

procedure TdxKmlListStyle.Parse(ANode: TdxXMLNode);
const
  DefaultListItemType: TdxKmlListItemType = kliCheck;
begin
  inherited;
  FBgColor := ParseColorElement(ANode, SdxKmlBgColor, $FFFFFFFF);
  FListItemType := ParseListItemType(ANode, SdxKmlListItemType, DefaultListItemType);
  CreateAndParseItemIcons(ANode);
end;

function TdxKmlListStyle.ParseListItemType(ANode: TdxXMLNode; const AElementName: TdxXMLString;
  ADefaultValue: TdxKmlListItemType): TdxKmlListItemType;
var
  AText: string;
begin
  AText := ParseStringElement(ANode, AElementName, '');
  if AText = SdxKmlCheckOffOnly then
    Result := kliCheckOffOnly
  else
    if AText = SdxKmlCheckHideChildren then
      Result := kliCheckHideChildren
    else
      if AText = SdxKmlRadioFolder then
        Result := kliRadioFolder
      else
        if AText = SdxKmlCheck then
          Result := kliCheck
        else
          Result := ADefaultValue;
end;

procedure TdxKmlListStyle.CreateAndParseItemIcons(ANode: TdxXMLNode);
begin
  FItemIconList := CreateAndParseElements(ANode, SdxKmlItemIcon);
end;

{ TdxKmlStyle }

destructor TdxKmlStyle.Destroy;
begin
  FreeAndNil(FIconStyle);
  FreeAndNil(FLabelStyle);
  FreeAndNil(FLineStyle);
  FreeAndNil(FPolyStyle);
  FreeAndNil(FBalloonStyle);
  FreeAndNil(FListStyle);
  inherited;
end;

class function TdxKmlStyle.GetParseName: string;
begin
  Result := SdxKmlStyle;
end;

procedure TdxKmlStyle.Parse(ANode: TdxXMLNode);
begin
  inherited Parse(ANode);
  FIconStyle := CreateAndParseElement(ANode, SdxKmlIconStyle) as TdxKmlIconStyle;
  FLabelStyle := CreateAndParseElement(ANode, SdxKmlLabelStyle) as TdxKmlLabelStyle;
  FLineStyle := CreateAndParseElement(ANode, SdxKmlLineStyle) as TdxKmlLineStyle;
  FPolyStyle := CreateAndParseElement(ANode, SdxKmlPolyStyle) as TdxKmlPolyStyle;
  FBalloonStyle := CreateAndParseElement(ANode, SdxKmlBalloonStyle) as TdxKmlBalloonStyle;
  FListStyle := CreateAndParseElement(ANode, SdxKmlListStyle) as TdxKmlListStyle;
end;

{ TdxKmlPair }

destructor TdxKmlPair.Destroy;
begin
  FreeAndNil(FStyle);
  inherited;
end;

class function TdxKmlPair.GetParseName: string;
begin
  Result := SdxKmlPair;
end;

procedure TdxKmlPair.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FKey := ParseKey(ANode, SdxKmlKey, kssNormal);
  FStyleUrl := ParseStringElement(ANode, SdxKmlStyleUrl, '');
  FStyle := CreateAndParseElement(ANode, SdxKmlStyle) as TdxKmlStyle;
end;

function TdxKmlPair.ParseKey(ANode: TdxXMLNode; const AElementName: TdxXMLString;
  ADefaultValue: TdxKmlStyleState): TdxKmlStyleState;
var
  AText: string;
begin
  AText := ParseStringElement(ANode, AElementName, '');
  if AText = SdxKmlNormalStyleState then
    Result := kssNormal
  else
    if AText = SdxKmlHighlightStyleState then
      Result := kssHighlight
    else
      Result := ADefaultValue;
end;

{ TdxKmlStyleMap }

constructor TdxKmlStyleMap.Create;
begin
  inherited;
end;

destructor TdxKmlStyleMap.Destroy;
begin
  FreeAndNil(FPairs);
  inherited;
end;

class function TdxKmlStyleMap.GetParseName: string;
begin
  Result := SdxKmlStyleMap;
end;

procedure TdxKmlStyleMap.Parse(ANode: TdxXMLNode);
begin
  inherited;
  FPairs := CreateAndParseElements(ANode, SdxKmlPair);
end;

{ TdxKmlRoot }

constructor TdxKmlRoot.Create;
begin
  inherited;
  FStyles := TdxFastObjectList.Create;
  FStyleMaps := TdxFastObjectList.Create;
  FPlacemarks := TdxFastObjectList.Create;
end;

destructor TdxKmlRoot.Destroy;
begin
  FreeAndNil(FPlacemarks);
  FreeAndNil(FStyleMaps);
  FreeAndNil(FStyles);
  inherited;
end;

function TdxKmlRoot.GetStyleIDFromUrl(const AUrl: string): string;
begin
  Result := StringReplace(AUrl, '#', '', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TdxKmlRoot.ResolveElementStyles(APlacemark: TdxKmlFeature;
  out ANormalStyle, AHighlightedStyle: TdxKmlStyle);
begin
  ANormalStyle := nil;
  AHighlightedStyle := nil;
  if APlacemark.StyleSelector <> nil then
  begin
    if APlacemark.StyleSelector is TdxKmlStyle then
      ANormalStyle := TdxKmlStyle(APlacemark.StyleSelector)
    else
      ResolveStyleMapStyles(TdxKmlStyleMap(APlacemark.StyleSelector), ANormalStyle, AHighlightedStyle);
  end
  else
    if APlacemark.StyleUrl <> '' then
      ResolveStylesByUrl(APlacemark.StyleUrl, ANormalStyle, AHighlightedStyle);
end;

procedure TdxKmlRoot.ResolveStyleByUrl(const AUrl: string;
  out AStyle: TdxKmlStyle);
var
  I: Integer;
  ACurrentStyle: TdxKmlStyle;
begin
  AStyle := nil;
  for I := 0 to Styles.Count - 1 do
  begin
    ACurrentStyle := Styles[I] as TdxKmlStyle;
    if ACurrentStyle.ID = GetStyleIDFromUrl(AUrl) then
    begin
      AStyle := ACurrentStyle;
      Break;
    end;
  end;
end;

procedure TdxKmlRoot.ResolveStylesByUrl(const AUrl: string;
  out ANormalStyle, AHighlightedStyle: TdxKmlStyle);
var
  I: Integer;
  AStyleMap: TdxKmlStyleMap;
begin
  ANormalStyle := nil;
  AHighlightedStyle := nil;
  ResolveStyleByUrl(AUrl, ANormalStyle);
  if ANormalStyle = nil then
    for I := 0 to StyleMaps.Count - 1 do
    begin
      AStyleMap := StyleMaps[I] as TdxKmlStyleMap;
      if AStyleMap.ID = GetStyleIDFromUrl(AUrl) then
        ResolveStyleMapStyles(AStyleMap, ANormalStyle, AHighlightedStyle);
    end;
end;

procedure TdxKmlRoot.ResolveStyleMapStyles(AStyleMap: TdxKmlStyleMap;
  out ANormalStyle, AHighlightedStyle: TdxKmlStyle);
var
  APairs: TdxFastObjectList;
  APair: TdxKmlPair;
  I: Integer;
begin
  ANormalStyle := nil;
  AHighlightedStyle := nil;
  APairs := AStyleMap.Pairs;
  for I := 0 to APairs.Count - 1 do
  begin
    APair := APairs[I] as TdxKmlPair;
    if APair.Key = kssNormal then
    begin
      if APair.Style <> nil then
        ANormalStyle := APair.Style
      else
        if APair.StyleUrl <> '' then
          ResolveStyleByUrl(APair.StyleUrl, ANormalStyle);
      Break;
    end;
  end;
  for I := 0 to APairs.Count - 1 do
  begin
    APair := APairs[I] as TdxKmlPair;
    if APair.Key = kssHighlight then
    begin
      if APair.Style <> nil then
        AHighlightedStyle := APair.Style
      else
        if APair.StyleUrl <> '' then
          ResolveStyleByUrl(APair.StyleUrl, AHighlightedStyle);
      Break;
    end;
  end;
end;

procedure TdxKmlRoot.Parse(ADocument: TdxXMLDocument);
begin
  Parse(ADocument.Root);
end;

procedure TdxKmlRoot.Parse(AFileStream: TStream);
var
  ADoc: TdxXMLDocument;
begin
  ADoc := TdxXMLDocument.Create(nil);
  try
    ADoc.LoadFromStream(AFileStream);
    Parse(ADoc);
  finally
    ADoc.Free;
  end;
end;

procedure TdxKmlRoot.Parse(ANode: TdxXMLNode);

  procedure DoParse(ANode: TdxXMLNode);
  begin
    ANode := ANode.First;
    while ANode <> nil do
    begin
      if ANode.Name = SdxKmlStyle then
        FStyles.Add(DoCreateAndParseElement(ANode))
      else

      if ANode.Name = SdxKmlStyleMap then
        FStyleMaps.Add(DoCreateAndParseElement(ANode))
      else

      if ANode.Name = SdxKmlPlacemark then
        FPlacemarks.Add(DoCreateAndParseElement(ANode))
      else
        DoParse(ANode);

      ANode := ANode.Next;
    end;
  end;

begin
  DoParse(ANode);
end;

initialization
  dxKmlElementsFactory.RegisterElements([TdxKmlPlacemark, TdxKmlStyle, TdxKmlStyleMap, TdxKmlPoint,
    TdxKmlLineString, TdxKmlLinearRing, TdxKmlOuterBoundaryIs, TdxKmlInnerBoundaryIs,
    TdxKmlPolygon, TdxKmlMultiGeometry, TdxKmlIcon, TdxKmlHotSpot, TdxKmlIconStyle,
    TdxKmlLabelStyle, TdxKmlLineStyle, TdxKmlPolyStyle, TdxKmlBalloonStyle,
    TdxKmlItemIcon, TdxKmlListStyle, TdxKmlPair]);

finalization
  FreeAndNil(FKmlElementsFactory);

end.
