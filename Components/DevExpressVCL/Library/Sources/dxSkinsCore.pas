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

unit dxSkinsCore;

{$I cxVer.inc}
{$MINENUMSIZE 1}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, Math, cxGraphics, cxGeometry, Generics.Collections, Generics.Defaults,
  cxClasses, cxLookAndFeels, dxGDIPlusApi, dxGDIPlusClasses, dxSkinsStrs, Forms,
  ActiveX, dxOffice11, dxCore, dxCoreGraphics, cxLookAndFeelPainters, dxSmartImage, dxDPIAwareUtils, Contnrs;

type
  TdxSkinVersion = Double;
  TdxSkinSignature = array[0..5] of AnsiChar;

  TdxSkinHeader = packed record
    Reserved: Integer;
    Signature: TdxSkinSignature;
    Version: TdxSkinVersion;
  end;

const
  dxSkinSignature: TdxSkinSignature = 'dxSkin';
  dxSkinStreamVersion: TdxSkinVersion = 1.25;
  dxSkinImageExt = '.png';
  dxSkinVectorImageExt = '.svg';

type
  TdxSkin = class;
  TdxSkinDetails = class;
  TdxSkinBooleanProperty = class;
  TdxSkinColor = class;
  TdxSkinControlGroup = class;
  TdxSkinControlGroupClass = class of TdxSkinControlGroup;
  TdxSkinCustomObject = class;
  TdxSkinCustomObjectClass = class of TdxSkinCustomObject;
  TdxSkinElement = class;
  TdxSkinElementCache = class;
  TdxSkinElementCacheList = class;
  TdxSkinElementClass = class of TdxSkinElement;
  TdxSkinImage = class;
  TdxSkinIntegerProperty = class;
  TdxSkinPersistentClass = class of TdxSkinPersistent;
  TdxSkinProperty = class;
  TdxSkinPropertyClass = class of TdxSkinProperty;

  TdxSkinGradientMode = (gmHorizontal, gmVertical, gmForwardDiagonal, gmBackwardDiagonal);
  TdxSkinIconSize = (sis16, sis48);
  TdxSkinImageSet = (imsDefault, imsOriginal, imsAlternate);

  TdxSkinObjectState = (sosUnassigned, sosUnused);
  TdxSkinObjectStates = set of TdxSkinObjectState;

  TdxSkinChange = (scStruct, scContent, scDetails);
  TdxSkinChanges = set of TdxSkinChange;

  TdxSkinChangeNotify = procedure (Sender: TObject; AChanges: TdxSkinChanges) of object;

  EdxSkinError = class(EdxException);

  { IdxSkinInfo }

  IdxSkinInfo = interface
  ['{97D85495-E631-413C-8DBC-BE7B784A9EA0}']
    function GetSkin: TdxSkin;
  end;

  { IdxSkinChangeListener }

  IdxSkinChangeListener = interface
  ['{28681774-0475-43AE-8704-1C904D294742}']
    procedure SkinChanged(Sender: TdxSkin);
  end;

  { IdxSkinChangeListener2 }

  IdxSkinChangeListener2 = interface
  ['{0D7C0942-D2C4-4579-AD03-A3CB5BBFC5AF}']
    procedure SkinChanged(ASkin: TdxSkin; AChanges: TdxSkinChanges);
  end;

  { IdxSkinColorPalette }

  IdxSkinColorPalette = interface
  ['{A7511E95-1577-41D3-ACB8-12F34E79AF1B}']
    function GetColor(const AKey: string; out AColor: TColor): Boolean;
  end;

  { TdxSkinCustomObject }

  TdxSkinCustomObject = class(TInterfacedPersistent)
  strict private
    FChanges: TdxSkinChanges;
    FName: string;
    FOwner: TPersistent;
    FState: TdxSkinObjectStates;
    FTag: NativeInt;
    FUpdateCount: Integer;

    FOnChange: TdxSkinChangeNotify;

    procedure SetName(const AValue: string);
  protected
    procedure AssignCore(Source: TPersistent); virtual;
    function GetOwner: TPersistent; override;
    procedure Changed(AChanges: TdxSkinChanges); virtual;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); virtual;
    procedure DataWrite(Stream: TStream); virtual;
    procedure DoChanged(AChanges: TdxSkinChanges); virtual;
    procedure FlushCache; virtual;
    //
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(AOwner: TPersistent; const AName: string); virtual;
    procedure Assign(Source: TPersistent); override; final;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    property State: TdxSkinObjectStates read FState write FState;
    property Tag: NativeInt read FTag write FTag;
  published
    property Name: string read FName write SetName;
    property OnChange: TdxSkinChangeNotify read FOnChange write FOnChange;
  end;

  { TdxSkinCustomObjectList }

  TdxSkinCustomObjectList = class abstract(TcxObjectList)
  strict private
    FChanges: TdxSkinChanges;
    FOwner: TPersistent;
    FSorted: Boolean;
    FUpdateCount: Integer;

    FOnChange: TdxSkinChangeNotify;

    class function CompareByName(AItem1, AItem2: TdxSkinCustomObject): Integer; static;
  protected
    procedure Changed(AChanges: TdxSkinChanges); virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure SubItemChanged(ASender: TObject; AChanges: TdxSkinChanges);

    procedure FlushCache;
    procedure SortChildren; virtual;

    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
    procedure DataReadItem(AStream: TStream; const AVersion: TdxSkinVersion); virtual; abstract;
    procedure DataWrite(AStream: TStream);
    procedure DataWriteItem(AStream: TStream; AItem: TdxSkinCustomObject); virtual; abstract;
    //
    property OnChange: TdxSkinChangeNotify read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent); virtual;
    constructor CreateEx(AOwner: TPersistent; AChangeHandler: TdxSkinChangeNotify);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; override;
    procedure Exchange(Index1, Index2: Integer);
    function Find(const AName: string; var AObject): Boolean;
    function IndexOf(const AName: string): Integer; overload;
    procedure Sort(ASortChildren: Boolean = False);
    //
    property Owner: TPersistent read FOwner;
  end;

  { TdxSkinProperties }

  TdxSkinProperties = class(TdxSkinCustomObjectList)
  strict private
    function GetItem(Index: Integer): TdxSkinProperty;
  protected
    procedure DataReadItem(AStream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWriteItem(AStream: TStream; AItem: TdxSkinCustomObject); override;
  public
    function Add(const AName: string; AClass: TdxSkinPropertyClass): TdxSkinProperty;
    procedure Assign(ASource: TdxSkinProperties);
    function Compare(AProperties: TdxSkinProperties): Boolean;
    //
    property Items[Index: Integer]: TdxSkinProperty read GetItem; default;
  end;

  { TdxSkinColors }

  TdxSkinColors = class(TdxSkinProperties)
  strict private
    function GetItem(Index: Integer): TdxSkinColor;
  public
    function Add(const AName: string; AValue: TColor): TdxSkinColor;
    //
    property Items[Index: Integer]: TdxSkinColor read GetItem; default;
  end;

  { TdxSkinControlGroups }

  TdxSkinControlGroups = class(TdxSkinCustomObjectList)
  strict private
    function GetItem(Index: Integer): TdxSkinControlGroup;
  public
    procedure DataReadItem(AStream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWriteItem(AStream: TStream; AItem: TdxSkinCustomObject); override;
    procedure SortChildren; override;
  public
    function Add(const AName: string): TdxSkinControlGroup;
    procedure Assign(ASource: TdxSkinControlGroups);
    procedure Dormant;
    //
    property Items[Index: Integer]: TdxSkinControlGroup read GetItem; default;
  end;

  { TdxSkinElements }

  TdxSkinElements = class(TdxSkinCustomObjectList)
  strict private
    function GetItem(Index: Integer): TdxSkinElement;
  protected
    procedure DataReadItem(AStream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWriteItem(AStream: TStream; AItem: TdxSkinCustomObject); override;
    procedure SortChildren; override;
  public
    function Add(const AName: string; AClass: TdxSkinElementClass): TdxSkinElement;
    procedure Assign(ASource: TdxSkinElements);
    procedure Dormant;
    //
    property Items[Index: Integer]: TdxSkinElement read GetItem; default;
  end;

  { TdxSkinColorValue }

  TdxSkinColorValue = packed record
  strict private const
    clUnassigned = $1FFFFFFE;
  strict private
    FDefaultValue: TColor;
    FOwner: TdxSkinCustomObject;
    FValue: TColor;
    FValueReference: string;

    function GetValue: TColor; inline;
    procedure SetValue(const AValue: TColor); inline;
    procedure SetValueReference(const AValue: string); inline;
  private
    procedure Changed;
    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
    procedure DataWrite(AStream: TStream);
  public
    constructor Create(AOwner: TdxSkinCustomObject; ADefaultValue: TColor = clDefault);
    procedure Assign(const ASource: TdxSkinColorValue);
    function Equals(const ASource: TdxSkinColorValue): Boolean;
    function GetColorPalette(out APalette: IdxSkinColorPalette): Boolean;
    procedure FlushCache;
    procedure ResetToDefaults;
    //
    property Value: TColor read GetValue write SetValue;
    property ValueReference: string read FValueReference write SetValueReference;
  end;

  { TdxSkinPersistent }

  TdxSkinPersistent = class(TdxSkinCustomObject)
  strict private
    function GetProperty(Index: Integer): TdxSkinProperty;
    function GetPropertyCount: Integer;
  protected
    FModified: Boolean;
    FProperties: TdxSkinProperties;

    procedure AssignCore(Source: TPersistent); override;
    procedure Changed(AChanges: TdxSkinChanges); override; final;
    procedure FlushCache; override;
    procedure SubItemChanged(ASender: TObject; AChanges: TdxSkinChanges);
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    destructor Destroy; override;
    //
    function AddProperty(const AName: string; APropertyClass: TdxSkinPropertyClass): TdxSkinProperty;
    function AddPropertyBool(const AName: string; AValue: Boolean): TdxSkinBooleanProperty;
    function AddPropertyColor(const AName: string; AValue: TColor): TdxSkinColor;
    function AddPropertyInteger(const AName: string; AValue: Integer): TdxSkinIntegerProperty;
    //
    procedure Clear; virtual;
    procedure DeleteProperty(const AProperty: TdxSkinProperty); overload; virtual;
    procedure DeleteProperty(const APropertyName: string); overload;
    procedure ExchangeProperty(AIndex1, AIndex2: Integer);
    function GetPropertyByName(const AName: string): TdxSkinProperty; overload;
    function GetPropertyByName(const AName: string; out AProperty: TdxSkinProperty): Boolean; overload;
    procedure Sort(ASortChildren: Boolean = False); virtual;
    //
    property Modified: Boolean read FModified write FModified;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[Index: Integer]: TdxSkinProperty read GetProperty;
  end;

  { TdxSkinProperty }

  TdxSkinProperty = class(TdxSkinCustomObject)
  public
    class procedure Register;
    class procedure Unregister;
    class function Description: string; virtual;
    function Compare(AProperty: TdxSkinProperty): Boolean; virtual;
  end;

  { TdxSkinIntegerProperty }

  TdxSkinIntegerProperty = class(TdxSkinProperty)
  strict private
    FValue: Integer;

    procedure SetValue(AValue: Integer);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
  public
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
  published
    property Value: Integer read FValue write SetValue default 0;
  end;

  { TdxSkinBooleanProperty }

  TdxSkinBooleanProperty = class(TdxSkinProperty)
  strict private
    FValue: Boolean;

    procedure SetValue(AValue: Boolean);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
  public
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
  published
    property Value: Boolean read FValue write SetValue default False;
  end;

  { TdxSkinColor }

  TdxSkinColorClass = class of TdxSkinColor;
  TdxSkinColor = class(TdxSkinProperty)
  strict private
    FValue: TdxSkinColorValue;

    function GetValue: TColor;
    function GetValueReference: string;
    procedure SetValue(const AValue: TColor);
    procedure SetValueReference(const Value: string);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
    procedure FlushCache; override;
    function GetValueAsAlphaColor: TdxAlphaColor; virtual;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
    property ValueAsAlphaColor: TdxAlphaColor read GetValueAsAlphaColor;
  published
    property Value: TColor read GetValue write SetValue;
    property ValueReference: string read GetValueReference write SetValueReference;
  end;

  { TdxSkinAlphaColor }

  TdxSkinAlphaColor = class(TdxSkinColor)
  strict private
    FValueAlpha: Byte;

    procedure SetValueAlpha(const Value: Byte);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: Double); override;
    procedure DataWrite(Stream: TStream); override;
    function GetValueAsAlphaColor: TdxAlphaColor; override;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
  published
    property ValueAlpha: Byte read FValueAlpha write SetValueAlpha default MaxByte;
  end;

  { TdxSkinRectProperty }

  TdxSkinRectProperty = class(TdxSkinProperty)
  strict private
    FValue: TcxRect;

    function GetValueByIndex(Index: Integer): Integer;
    procedure SetValue(Value: TcxRect);
    procedure SetValueByIndex(Index, Value: Integer);
    procedure InternalHandler(Sender: TObject);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    destructor Destroy; override;
    function Compare(AProperty: TdxSkinProperty): Boolean; override;

    property Value: TcxRect read FValue write SetValue;
  published
    property Left: Integer index 0 read GetValueByIndex write SetValueByIndex default 0;
    property Top: Integer index 1 read GetValueByIndex write SetValueByIndex default 0;
    property Right: Integer index 2 read GetValueByIndex write SetValueByIndex default 0;
    property Bottom: Integer index 3 read GetValueByIndex write SetValueByIndex default 0;
  end;

  { TdxSkinSizeProperty }

  TdxSkinSizeProperty = class(TdxSkinProperty)
  strict private
    FValue: TSize;

    procedure SetValue(const AValue: TSize);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
  public
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
    function GetValueByIndex(Index: Integer): Integer;
    procedure SetValueByIndex(Index, Value: Integer);

    property Value: TSize read FValue write SetValue;
  published
    property cx: Integer index 0 read GetValueByIndex write SetValueByIndex default 0;
    property cy: Integer index 1 read GetValueByIndex write SetValueByIndex default 0;
  end;

  { TdxSkinBorder }

  TdxSkinBorder = class(TdxSkinProperty)
  strict private
    FColor: TdxSkinColorValue;
    FKind: TcxBorder;
    FThin: Integer;

    function GetColor: TColor;
    function GetColorReference: string;
    function GetContentMargin: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetColorReference(const Value: string);
    procedure SetThin(AValue: Integer);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
    procedure FlushCache; override;
  public
    constructor Create(AOwner: TPersistent; AKind: TcxBorder); reintroduce; virtual;
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
    procedure Draw(DC: HDC; const ABounds: TRect); virtual;
    procedure DrawEx(ACanvas: TdxGPCanvas; const ABounds: TRect); virtual;
    procedure ResetToDefaults; virtual;

    property ContentMargin: Integer read GetContentMargin;
    property Kind: TcxBorder read FKind;
  published
    property Color: TColor read GetColor write SetColor default clNone;
    property ColorReference: string read GetColorReference write SetColorReference;
    property Thin: Integer read FThin write SetThin default 1;
  end;

  { TdxSkinBorders }

  TdxSkinBorders = class(TdxSkinProperty)
  strict private
    FBorders: array[TcxBorder] of TdxSkinBorder;

    function GetBorder(ABorder: TcxBorder): TdxSkinBorder;
    function GetBorderByIndex(Index: Integer): TdxSkinBorder;
    function GetContentMargins: TRect;
    procedure SetBorderByIndex(Index: Integer; AValue: TdxSkinBorder);
    procedure SubItemChanged(ASender: TObject; AChanges: TdxSkinChanges);
  protected
    procedure AssignCore(ASource: TPersistent); override;
    procedure CreateBorders;
    procedure DeleteBorders;
    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(AStream: TStream); override;
    procedure FlushCache; override;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    destructor Destroy; override;
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
    procedure Draw(ACanvas: TdxGPCanvas; const ABounds: TRect); virtual;
    procedure ResetToDefaults; virtual;
    //
    property ContentMargins: TRect read GetContentMargins;
    property Items[AKind: TcxBorder]: TdxSkinBorder read GetBorder; default;
  published
    property Left: TdxSkinBorder index 0 read GetBorderByIndex write SetBorderByIndex;
    property Top: TdxSkinBorder index 1 read GetBorderByIndex write SetBorderByIndex;
    property Right: TdxSkinBorder index 2 read GetBorderByIndex write SetBorderByIndex;
    property Bottom: TdxSkinBorder index 3 read GetBorderByIndex write SetBorderByIndex;
  end;

  { TdxSkinStringProperty }

  TdxSkinStringProperty = class(TdxSkinProperty)
  strict private
    FValue: string;

    procedure SetValue(const AValue: string);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
  public
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
  published
    property Value: string read FValue write SetValue;
  end;

  { TdxSkinWideStringProperty }

  TdxSkinWideStringProperty = class(TdxSkinStringProperty)
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(Stream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(Stream: TStream); override;
  end;

  { TdxSkinAlternateImageAttributes }

  TdxSkinAlternateImageAttributes = class(TdxSkinProperty)
  strict private
    FAlpha: Byte;
    FBorders: TdxSkinBorders;
    FBordersInner: TdxSkinBorders;
    FContentOffsets: TcxRect;
    FGradient: TdxSkinGradientMode;
    FGradientBeginColor: TdxSkinColorValue;
    FGradientEndColor: TdxSkinColorValue;

    procedure BordersChanged(ASender: TObject; AChanges: TdxSkinChanges);
    procedure ContentOffsetsChanged(ASender: TObject);
    function GetGradientBeginColor: TColor;
    function GetGradientBeginColorReference: string;
    function GetGradientEndColor: TColor;
    function GetGradientEndColorReference: string;
    function GetIsAlphaUsed: Boolean;
    procedure SetAlpha(AValue: Byte);
    procedure SetBorders(AValue: TdxSkinBorders);
    procedure SetBordersInner(AValue: TdxSkinBorders);
    procedure SetContentOffsets(AValue: TcxRect);
    procedure SetGradientBeginColor(AValue: TColor);
    procedure SetGradientBeginColorReference(const Value: string);
    procedure SetGradientEndColor(AValue: TColor);
    procedure SetGradientEndColorReference(const Value: string);
    procedure SetGradientMode(AValue: TdxSkinGradientMode);
  protected
    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(AStream: TStream); override;
    procedure FlushCache; override;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TdxGPCanvas; const R: TRect); virtual;
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
    //
    property IsAlphaUsed: Boolean read GetIsAlphaUsed;
  published
    property Alpha: Byte read FAlpha write SetAlpha default 255;
    property Borders: TdxSkinBorders read FBorders write SetBorders;
    property BordersInner: TdxSkinBorders read FBordersInner write SetBordersInner;
    property ContentOffsets: TcxRect read FContentOffsets write SetContentOffsets;
    property Gradient: TdxSkinGradientMode read FGradient write SetGradientMode default gmHorizontal;
    property GradientBeginColor: TColor read GetGradientBeginColor write SetGradientBeginColor default clNone;
    property GradientBeginColorReference: string read GetGradientBeginColorReference write SetGradientBeginColorReference;
    property GradientEndColor: TColor read GetGradientEndColor write SetGradientEndColor default clNone;
    property GradientEndColorReference: string read GetGradientEndColorReference write SetGradientEndColorReference;
  end;

  { TdxSkinCustomListProperty }

  TdxSkinCustomListProperty = class(TdxSkinProperty)
  strict private
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxSkinProperty;
  protected
    FProperties: TdxSkinProperties;

    procedure AssignCore(Source: TPersistent); override;
    function CreateProperties: TdxSkinProperties; virtual;
    procedure DataRead(Stream: TStream; const AVersion: Double); override;
    procedure DataWrite(Stream: TStream); override;
    procedure FlushCache; override;
    procedure HandlerPropertiesChanged(Sender: TObject; Changes: TdxSkinChanges);
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Clear;
    function Compare(AProperty: TdxSkinProperty): Boolean; override;
    procedure Delete(const AProperty: TdxSkinProperty); overload;
    procedure Delete(const AIndex: Integer); overload;
    procedure Delete(const AName: string); overload;
    function Find(const AName: string; var AProperty): Boolean;
    procedure Sort(ASortChildren: Boolean = False);
    //
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxSkinProperty read GetItem; default;
  end;

  { TdxSkinColorPalette }

  TdxSkinColorPalette = class(TdxSkinCustomListProperty, IdxColorPalette)
  strict private
    FID: TGUID;

    function GetItem(Index: Integer): TdxSkinColor;
  protected
    FDefaultColorClass: TdxSkinColorClass;

    function AddCore(const AName: string; AValue: TColor; AValueReference: string): TdxSkinColor;
    procedure DoChanged(AChanges: TdxSkinChanges); override;
    procedure FlushCache; override;
    // IdxColorPalette
    function GetID: TGUID;
    function GetFillColor(const ID: string): TdxAlphaColor;
    function GetStrokeColor(const ID: string): TdxAlphaColor;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    function Add(const AName: string; AValue: TColor): TdxSkinColor; overload;
    function Add(const AName, AValueReference: string): TdxSkinColor; overload;
    //
    property Items[Index: Integer]: TdxSkinColor read GetItem; default;
  end;

  { TdxSkinColorPalettes }

  TdxSkinColorPalettes = class(TdxSkinCustomListProperty)
  strict private
    function GetItem(Index: Integer): TdxSkinColorPalette;
  protected
    FDefaultColorClass: TdxSkinColorClass;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    function Add(const AName: string): TdxSkinColorPalette;
    //
    property Items[Index: Integer]: TdxSkinColorPalette read GetItem; default;
  end;

  { TdxSkinControlGroup }

  TdxSkinControlGroup = class(TdxSkinPersistent)
  strict private
    function GetCount: Integer;
    function GetElement(AIndex: Integer): TdxSkinElement;
    function GetHasMissingElements: Boolean;
    function GetSkin: TdxSkin;
    procedure SetElement(AIndex: Integer; AElement: TdxSkinElement);
  protected
    FElements: TdxSkinElements;

    procedure AssignCore(Source: TPersistent); override;
    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(AStream: TStream); override;
    procedure FlushCache; override;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    destructor Destroy; override;
    function AddElement(const AName: string): TdxSkinElement;
    function AddElementEx(const AName: string; AElementClass: TdxSkinElementClass): TdxSkinElement;
    function GetElementByName(const AName: string): TdxSkinElement; overload;
    function GetElementByName(const AName: string; out AElement: TdxSkinElement): Boolean; overload;
    procedure Clear; override;
    procedure ClearModified;
    procedure Delete(AIndex: Integer);
    procedure Dormant; virtual;
    procedure ExchangeElements(AIndex1, AIndex2: Integer);
    procedure RemoveElement(AElement: TdxSkinElement);
    procedure RemoveElementByName(const AElementName: string);
    procedure Sort(ASortChildren: Boolean = False); override;
    //
    property Count: Integer read GetCount;
    property Elements[Index: Integer]: TdxSkinElement read GetElement write SetElement;
    property HasMissingElements: Boolean read GetHasMissingElements;
    property Skin: TdxSkin read GetSkin;
  end;

  { TdxSkinImage }

  TdxSkinElementState = (esNormal, esHot, esPressed, esDisabled, esActive,
    esFocused, esDroppedDown, esChecked, esHotCheck, esActiveDisabled, esCheckPressed);
  TdxSkinElementStates = set of TdxSkinElementState;

  TdxSkinImagePart = (
    sipTopLeft,    sipTop,    sipTopRight,
    sipLeft,       sipCenter, sipRight,
    sipBottomLeft, sipBottom, sipBottomRight
  );

  TdxSkinElementPartBounds = array[TdxSkinImagePart] of TRect;
  TdxSkinElementPartVisibility = array[TdxSkinImagePart] of Boolean;

  TdxSkinImageLayout = (ilHorizontal, ilVertical);
  TdxSkinStretchMode = (smStretch, smTile, smNoResize);

  TdxSkinImage = class(TPersistent)
  strict private
    FColorPalettes: TdxSkinColorPalettes;
    FGradient: TdxSkinGradientMode;
    FGradientBeginColor: TdxSkinColorValue;
    FGradientEndColor: TdxSkinColorValue;
    FImageCount: Integer;
    FImageLayout: TdxSkinImageLayout;
    FInterpolationMode: TdxGPInterpolationMode;
    FIsDirty: Boolean;
    FMargins: TcxMargin;
    FOwner: TdxSkinElement;
    FPartsBounds: TdxSkinElementPartBounds;
    FPartsBoundsSourceSize: TSize;
    FPartsVisibility: TdxSkinElementPartVisibility;
    FScaleMargins: Boolean;
    FSize: TSize;
    FSourceName: string;
    FStateBounds: array[TdxSkinElementState] of TRect;
    FStateCount: Integer;
    FStates: TdxSkinElementStates;
    FStretch: TdxSkinStretchMode;
    FTexture: TdxSmartImage;
    FTextureIsVector: TdxDefaultBoolean;

    FOnChange: TNotifyEvent;

    function GetEmpty: Boolean;
    function GetFileExt: string;
    function GetGradientBeginColor: TColor;
    function GetGradientBeginColorReference: string;
    function GetGradientEndColor: TColor;
    function GetGradientEndColorReference: string;
    function GetIsGradientParamsAssigned: Boolean; inline;
    function GetIsVectorTexture: Boolean; inline;
    function GetName: string;
    function GetPartBounds(APart: TdxSkinImagePart): TRect;
    function GetPartVisible(APart: TdxSkinImagePart): Boolean;
    function GetSize: TSize;
    function GetSourceName: string;
    function GetStateBounds(AImageIndex: Integer; AState: TdxSkinElementState): TRect;
    function GetStateCount: Integer;
    procedure SetColorPalettes(const Value: TdxSkinColorPalettes);
    procedure SetGradientBeginColor(AValue: TColor);
    procedure SetGradientBeginColorReference(const Value: string);
    procedure SetGradientEndColor(AValue: TColor);
    procedure SetGradientEndColorReference(const Value: string);
    procedure SetGradientMode(AValue: TdxSkinGradientMode);
    procedure SetImageCount(AValue: Integer);
    procedure SetImageLayout(AValue: TdxSkinImageLayout);
    procedure SetInterpolationMode(AValue: TdxGPInterpolationMode);
    procedure SetMargins(AValue: TcxMargin);
    procedure SetName(const AValue: string);
    procedure SetStates(AValue: TdxSkinElementStates);
    procedure SetStretch(AValue: TdxSkinStretchMode);
    procedure HandlerColorPaletteChanged(Sender: TObject; Changes: TdxSkinChanges);
    procedure HandlerMarginsChanged(Sender: TObject);
    procedure HandlerTextureChanged(Sender: TObject);
  protected
    function CreateTexture: TdxSmartImage; virtual;
    function GetOwner: TPersistent; override;
    procedure Changed; virtual;
    procedure CheckCalculatePartsBounds(const ASize: TSize);
    procedure CheckInfo;
    procedure CheckState(var AState: TdxSkinElementState; var AImageIndex: Integer);
    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion); virtual;
    procedure DataWrite(AStream: TStream); virtual;
    procedure FlushCache; virtual;
    function GetPalette(AState: TdxSkinElementState; ACanUseGlobalPalette: Boolean = True): IdxColorPalette;
    function GetSourceDPI: Integer; virtual;
    procedure InitializeInfo; virtual;

    property IsDirty: Boolean read FIsDirty write FIsDirty;
    property IsGradientParamsAssigned: Boolean read GetIsGradientParamsAssigned;
    property IsVectorTexture: Boolean read GetIsVectorTexture;
    property PartBounds[APart: TdxSkinImagePart]: TRect read GetPartBounds;
    property PartVisible[APart: TdxSkinImagePart]: Boolean read GetPartVisible;
    property ScaleMargins: Boolean read FScaleMargins write FScaleMargins;
  public
    constructor Create(AOwner: TdxSkinElement); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function Compare(AImage: TdxSkinImage): Boolean; virtual;
    procedure Dormant; virtual;

    procedure Draw(DC: HDC; const ARect: TRect;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload;
    procedure Draw(DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload; virtual;
    procedure DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload;
    procedure DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload; virtual;
    procedure DrawRTL(DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); virtual;

    procedure GetBitmap(AImageIndex: Integer; AState: TdxSkinElementState; ABitmap: TBitmap; ABackgroundColor: TColor = clNone);
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromResource(AInstance: THandle; const AName: string; AType: PChar);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SetStateMapping(ATargetStateOrder: array of TdxSkinElementState);

    property Empty: Boolean read GetEmpty;
    property Name: string read GetName write SetName;
    property Owner: TdxSkinElement read FOwner;
    property Size: TSize read GetSize;
    property SourceName: string read GetSourceName;
    property StateBounds[ImageIndex: Integer; State: TdxSkinElementState]: TRect read GetStateBounds;
    property StateCount: Integer read GetStateCount;
    property Texture: TdxSmartImage read FTexture;
  published
    property ColorPalettes: TdxSkinColorPalettes read FColorPalettes write SetColorPalettes;
    property Gradient: TdxSkinGradientMode read FGradient write SetGradientMode default gmHorizontal;
    property GradientBeginColor: TColor read GetGradientBeginColor write SetGradientBeginColor default clNone;
    property GradientBeginColorReference: string read GetGradientBeginColorReference write SetGradientBeginColorReference;
    property GradientEndColor: TColor read GetGradientEndColor write SetGradientEndColor default clNone;
    property GradientEndColorReference: string read GetGradientEndColorReference write SetGradientEndColorReference;
    property ImageCount: Integer read FImageCount write SetImageCount default 1;
    property ImageLayout: TdxSkinImageLayout read FImageLayout write SetImageLayout default ilHorizontal;
    property InterpolationMode: TdxGPInterpolationMode read FInterpolationMode write SetInterpolationMode default imDefault;
    property Margins: TcxMargin read FMargins write SetMargins;
    property States: TdxSkinElementStates read FStates write SetStates default [esNormal];
    property Stretch: TdxSkinStretchMode read FStretch write SetStretch default smStretch;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxSkinGlyph }

  TdxSkinGlyph = class(TdxSkinImage)
  strict private
    function GetTexture: TdxSmartGlyph;
  protected
    function CreateTexture: TdxSmartImage; override;
    function GetSourceDPI: Integer; override;
  public
    procedure AfterConstruction; override;
    //
    property Texture: TdxSmartGlyph read GetTexture;
  end;

  { TdxSkinElement }

  TdxSkinAlternateImageSet = array[TdxSkinElementState] of TdxSkinAlternateImageAttributes;

  TdxSkinElement = class(TdxSkinPersistent)
  strict private
    FAlpha: Byte;
    FAlternateImageSetDirty: Boolean;
    FAlternateImageSetIndex: Integer;
    FBorders: TdxSkinBorders;
    FCache: TdxSkinElementCacheList;
    FCacheCapacity: Integer;
    FColor: TdxSkinColorValue;
    FContentOffset: TcxRect;
    FGlyph: TdxSkinImage;
    FImage: TdxSkinImage;
    FMinSize: TcxSize;
    FTextColor: TdxSkinColorValue;

    procedure BordersChanged(ASender: TObject; AChanges: TdxSkinChanges);
    function GetColor: TColor;
    function GetColorReference: string;
    function GetGroup: TdxSkinControlGroup;
    function GetImageCount: Integer;
    function GetIsAlphaUsed: Boolean;
    function GetPath: string;
    function GetSize: TSize;
    function GetStates: TdxSkinElementStates;
    function GetTextColorPropertyValue: TColor;
    function GetTextColorReference: string;
    function GetUseCache: Boolean;
    procedure SetAlpha(AValue: Byte);
    procedure SetBorders(AValue: TdxSkinBorders);
    procedure SetCacheCapacity(AValue: Integer);
    procedure SetColor(AValue: TColor);
    procedure SetColorReference(const Value: string);
    procedure SetContentOffset(AValue: TcxRect);
    procedure SetGlyph(AValue: TdxSkinImage);
    procedure SetImage(AValue: TdxSkinImage);
    procedure SetMinSize(AValue: TcxSize);
    procedure SetTextColorPropertyValue(AValue: TColor);
    procedure SetTextColorReference(const Value: string);
    procedure SetUseCache(AValue: Boolean);
    procedure SubItemChanged(ASender: TObject);
  protected
    FAlternateImageSet: TdxSkinAlternateImageSet;
    FReadImageCount: Integer;

    procedure AssignCore(Source: TPersistent); override;
    function CanUseAlternateImageSet(AImageIndex: Integer; AState: TdxSkinElementState;
      ALowColorsMode: Boolean; out AStateAttributes: TdxSkinAlternateImageAttributes): Boolean;
    procedure CheckAlternateImageSet(AIndex: Integer);
    procedure DoChanged(AChanges: TdxSkinChanges); override;
    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion); override;
    procedure DataWrite(AStream: TStream); override;
    procedure FlushCache; override;
    //
    property AlternateImageSetDirty: Boolean read FAlternateImageSetDirty write FAlternateImageSetDirty;
    property AlternateImageSetIndex: Integer read FAlternateImageSetIndex;
    property Cache: TdxSkinElementCacheList read FCache;
  public
    constructor Create(AOwner: TPersistent; const AName: string); override;
    destructor Destroy; override;
    function AddAlternateImageAttributes(AState: TdxSkinElementState; AImageIndex: Integer = 0): TdxSkinAlternateImageAttributes;
    function Compare(AElement: TdxSkinElement): Boolean; virtual;
    function CalculateMinSize: TSize;
    function GetGlyphColorPalette(AState: TdxSkinElementState): IdxColorPalette;
    function GetTextColor(AState: TcxButtonState; const APropertyPrefix: string = ''): TColor; overload; virtual;
    procedure Dormant; virtual;

    procedure Draw(DC: HDC; const ARect: TRect;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload;
    procedure Draw(DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload; virtual;
    procedure DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload;
    procedure DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); overload; virtual;
    procedure DrawRTL(DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);

    function GetImage(AImageIndex: Integer; ASize: TSize; AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal): TdxSmartImage; overload;
    function GetImage(AImageIndex: Integer; const ASize: TSize; AState: TdxSkinElementState = esNormal): TdxSmartImage; overload;
    procedure SetStateMapping(AStateOrder: array of TdxSkinElementState);
    //
    property Group: TdxSkinControlGroup read GetGroup;
    property ImageCount: Integer read GetImageCount;
    property IsAlphaUsed: Boolean read GetIsAlphaUsed;
    property Path: string read GetPath;
    property Size: TSize read GetSize;
    property States: TdxSkinElementStates read GetStates;
    //
    property CacheCapacity: Integer read FCacheCapacity write SetCacheCapacity;
    property UseCache: Boolean read GetUseCache write SetUseCache;
  published
    property Alpha: Byte read FAlpha write SetAlpha default 255;
    property Borders: TdxSkinBorders read FBorders write SetBorders;
    property Color: TColor read GetColor write SetColor default clDefault;
    property ColorReference: string read GetColorReference write SetColorReference;
    property ContentOffset: TcxRect read FContentOffset write SetContentOffset;
    property Glyph: TdxSkinImage read FGlyph write SetGlyph;
    property Image: TdxSkinImage read FImage write SetImage;
    property MinSize: TcxSize read FMinSize write SetMinSize;
    property TextColor: TColor read GetTextColorPropertyValue write SetTextColorPropertyValue default clDefault;
    property TextColorReference: string read GetTextColorReference write SetTextColorReference;
  end;

  { TdxSkinEmptyElement }

  TdxSkinEmptyElement = class(TdxSkinElement)
  public
    procedure Draw(DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal); override;
  end;

  { TdxSkinElementCache }

  TdxSkinElementCache = class
  strict private
    FCache: GpBitmap;
    FCacheOpaque: TcxBitmap;
    FElement: TdxSkinElement;
    FImageIndex: Integer;
    FImageSet: TdxSkinImageSet;
    FRect: TRect;
    FScaleFactor: TdxScaleFactor;
    FState: TdxSkinElementState;

    procedure FreeCache;
    procedure InitCache(R: TRect);
    procedure InitOpaqueCache(const R: TRect);
  protected
    property Element: TdxSkinElement read FElement;
    property ImageIndex: Integer read FImageIndex;
    property Rect: TRect read FRect;
    property State: TdxSkinElementState read FState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckCacheState(AElement: TdxSkinElement; const R: TRect;
      AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure CheckCacheState(AElement: TdxSkinElement; const R: TRect; AScaleFactor: TdxScaleFactor;
      AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure Draw(DC: HDC; const R: TRect); overload;
    procedure Draw(ACanvas: TdxGPCanvas; const R: TRect); overload;
    procedure DrawEx(DC: HDC; AElement: TdxSkinElement; const R: TRect;
      AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure DrawEx(DC: HDC; AElement: TdxSkinElement; const R: TRect; AScaleFactor: TdxScaleFactor;
      AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure DrawEx(ACanvas: TdxGPCanvas; AElement: TdxSkinElement; const R: TRect;
      AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure Flush;
  end;

  { TdxSkinElementCacheList }

  TdxSkinElementCacheList = class
  public const
    DefaultCapacity = 8;
  strict private
    FCapacity: Integer;
    FData: TObjectList;

    procedure CheckCapacity;
    procedure SetCapacity(AValue: Integer);
  protected
    function GetCacheItem(AElement: TdxSkinElement; const R: TRect;
      AState: TdxSkinElementState; AImageIndex: Integer): TdxSkinElementCache;
    function TryGet(AElement: TdxSkinElement; const R: TRect; AState: TdxSkinElementState;
      AImageIndex: Integer; out AElementCache: TdxSkinElementCache): Boolean;
  public
    constructor Create(ACapacity: Integer = DefaultCapacity);
    destructor Destroy; override;
    procedure DrawElement(DC: HDC; AElement: TdxSkinElement; const R: TRect;
      AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure DrawElement(DC: HDC; AElement: TdxSkinElement; const R: TRect; AScaleFactor: TdxScaleFactor;
      AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure DrawElement(ACanvas: TdxGPCanvas; AElement: TdxSkinElement; const R: TRect;
      AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0); overload;
    procedure Flush;
    //
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  { TdxSkinBinaryWriter }

  TdxSkinBinaryWriter = class
  strict private
    FCount: Integer;
    FHeaderOffset: Int64;
    FStream: TStream;

    procedure WriteHeader;
  protected
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure AddSkin(ASkin: TdxSkin);
  end;

  { TdxSkinBinaryReader }

  TdxSkinBinaryReader = class
  strict private
    FSkins: TcxObjectList;
    FStream: TStream;

    function GetCount: Integer;
    function GetSkinDetails(Index: Integer): TdxSkinDetails;
    function GetSkinDisplayName(Index: Integer): string;
    function GetSkinName(Index: Integer): string;
    function GetSkinOffset(Index: Integer): Integer;
  protected
    function IndexOf(const ASkinName: string): Integer;
    function ReadBinaryProject(AStream: TStream): Boolean;
    function ReadBinarySkin(AStream: TStream): Boolean;
    procedure ReadSkinsInfo;
    //
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream); virtual;
    destructor Destroy; override;
    function LoadSkin(ASkin: TdxSkin; ASkinIndex: Integer): Boolean; overload;
    function LoadSkin(ASkin: TdxSkin; const ASkinName: string): Boolean; overload;
    //
    property Count: Integer read GetCount;
    property SkinDetails[Index: Integer]: TdxSkinDetails read GetSkinDetails;
    property SkinDisplayName[Index: Integer]: string read GetSkinDisplayName;
    property SkinName[Index: Integer]: string read GetSkinName;
    property SkinOffset[Index: Integer]: Integer read GetSkinOffset;
  end;

  { TdxSkinDetails }

  TdxSkinDetails = class(TPersistent)
  strict private
    FDisplayName: string;
    FGroupName: string;
    FIcons: array [TdxSkinIconSize] of TdxSmartImage;
    FName: string;
    FNotes: WideString;
    FUpdateCount: Integer;

    FOnChange: TNotifyEvent;

    function GetIcon(ASize: TdxSkinIconSize): TdxSmartImage;
    procedure DoIconsChanged(Sender: TObject);
    procedure SetDisplayName(const AValue: string);
    procedure SetGroupName(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetNotes(const AValue: WideString);
  protected
    FDataOffset: Int64;

    procedure Changed; virtual;
    procedure DataRead(AStream: TStream; const AVersion: TdxSkinVersion); virtual;
    procedure DataWrite(AStream: TStream); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    function LoadFromStream(AStream: TStream): Boolean;
    procedure ResetIcon(ASize: TdxSkinIconSize);
    //
    property DisplayName: string read FDisplayName write SetDisplayName;
    property GroupName: string read FGroupName write SetGroupName;
    property Icons[ASize: TdxSkinIconSize]: TdxSmartImage read GetIcon;
    property Name: string read FName write SetName;
    property Notes: WideString read FNotes write SetNotes;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxSkin }

  TdxSkinClass = class of TdxSkin;
  TdxSkin = class(TdxSkinPersistent,
    IdxColorPalette,
    IdxSkinColorPalette)
  strict private
    FColorPalette: TdxSkinColorPalette;
    FColorPalettes: TdxSkinColorPalettes;
    FDestroying: Boolean;
    FDetails: TdxSkinDetails;
    FGroups: TdxSkinControlGroups;
    FListeners: TInterfaceList;

    FOnChange: TdxSkinChangeNotify;

    function GetColor(Index: Integer): TdxSkinColor;
    function GetColorCount: Integer;
    function GetActiveColorPaletteName: string;
    function GetDisplayName: string;
    function GetGroup(Index: Integer): TdxSkinControlGroup;
    function GetGroupCount: Integer;
    function GetHasMissingElements: Boolean;
    function GetName: string;
    procedure SetActiveColorPaletteName(const Value: string);
    procedure SetName(const Value: string);

    procedure HandlerColorPalettesChanged(Sender: TObject; AChanges: TdxSkinChanges);
    procedure HandlerDetailsChanged(Sender: TObject);
  protected
    FColors: TdxSkinColors;

    procedure AssignCore(Source: TPersistent); override;
    procedure FlushCache; override;
    procedure DoChanged(AChanges: TdxSkinChanges); override;
    procedure LoadFromResource(hInst: THandle); virtual;
    function GetGlyphColorPalette(AState: TdxSkinElementState): IdxColorPalette;
    procedure NotifyListener(AChanges: TdxSkinChanges; const ACustomListener: IUnknown);
    procedure NotifyListeners(AChanges: TdxSkinChanges);
    procedure SelectColorPalette(const Value: string);
    // IdxSkinColorPalette
    function IdxSkinColorPalette.GetColor = PaletteGetColor;
    function PaletteGetColor(const AKey: string; out AColor: TColor): Boolean;

    property Listeners: TInterfaceList read FListeners;
  public
    constructor Create(const AName: string; ALoadOnCreate: Boolean; hInst: THandle); reintroduce; virtual;
    destructor Destroy; override;
    function AddColor(const AName: string; const AColor: TColor = clDefault): TdxSkinColor;
    function AddGroup(const AName: string = ''): TdxSkinControlGroup;
    procedure Clear; override;
    procedure ClearModified;
    function Clone(const AName: string): TdxSkin; reintroduce; virtual;
    procedure DeleteGroup(const AGroup: TdxSkinControlGroup); virtual;
    procedure DeleteProperty(const AProperty: TdxSkinProperty); override;
    procedure Dormant; virtual;
    procedure ExchangeColors(AIndex1, AIndex2: Integer);
    procedure ExchangeGroups(AIndex1, AIndex2: Integer);
    function GetColorByName(const AName: string): TdxSkinColor;
    function GetGroupByName(const AName: string): TdxSkinControlGroup; overload;
    function GetGroupByName(const AName: string; out AGroup: TdxSkinControlGroup): Boolean; overload;
    procedure Sort(ASortChildren: Boolean = False); override;
    //
    procedure LoadFromBinaryFile(const AFileName: string);
    procedure LoadFromBinaryStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream); virtual;
    procedure SaveToBinaryFile(const AFileName: string);
    procedure SaveToBinaryStream(AStream: TStream);
    //
    procedure AddListener(AListener: IUnknown);
    procedure RemoveListener(AListener: IUnknown);
    //
    property ActiveColorPalette: TdxSkinColorPalette read FColorPalette implements IdxColorPalette;
    property ActiveColorPaletteName: string read GetActiveColorPaletteName write SetActiveColorPaletteName;
    property ColorCount: Integer read GetColorCount;
    property Colors[Index: Integer]: TdxSkinColor read GetColor;
    property ColorPalettes: TdxSkinColorPalettes read FColorPalettes;
    property Details: TdxSkinDetails read FDetails;
    property GroupCount: Integer read GetGroupCount;
    property Groups[Index: Integer]: TdxSkinControlGroup read GetGroup;
    property HasMissingElements: Boolean read GetHasMissingElements;
  published
    property DisplayName: string read GetDisplayName;
    property Name: string read GetName write SetName;
    //
    property OnChange: TdxSkinChangeNotify read FOnChange write FOnChange;
  end;

const
  dxSkinElementTextColorPropertyNames: array[TcxButtonState] of string = (
    '', sdxTextColorNormal, sdxTextColorHot, sdxTextColorPressed, sdxTextColorDisabled
  );

  dxSkinElementStateNames: array[TdxSkinElementState] of string = (
    'Normal', 'Hot', 'Pressed', 'Disabled',  'Active', 'Focused',
    'DroppedDown', 'Checked', 'HotCheck', 'ActiveDisabled', 'CheckPressed'
  );

  dxSkinsGradientModeMap: array[TdxSkinGradientMode] of TdxGPLinearGradientMode = (
    LinearGradientModeHorizontal, LinearGradientModeVertical,
    LinearGradientModeForwardDiagonal, LinearGradientModeBackwardDiagonal
  );

var
  dxSkinsUseImageSet: TdxSkinImageSet = imsDefault;

function dxSkinRegisteredPropertyTypes: TList;

procedure dxSkinInvalidOperation(const AMessage: string);
procedure dxSkinCheck(ACondition: Boolean; const AMessage: string);
function dxSkinCheckSignature(AStream: TStream; out AVersion: TdxSkinVersion): Boolean;
function dxSkinCheckSkinElement(AElement: TdxSkinElement): TdxSkinElement;
procedure dxSkinCheckVersion(const AVersion: TdxSkinVersion);
procedure dxSkinWriteSignature(AStream: TStream);

procedure dxSkinsCalculatePartsBounds(const R, AMargins: TRect; var AParts);
procedure dxSkinsCheckMargins(var AMargins: TRect; const R: TRect);

function dxSkinGetAlternateImageAttrsPropertyName(AState: TdxSkinElementState; AImageIndex: Integer = 0): string;

function dxSkinReadStringFromStream(AStream: TStream): string;
function dxSkinReadWideStringFromStream(AStream: TStream): WideString;
procedure dxSkinWriteStringToStream(AStream: TStream; const AValue: string);
procedure dxSkinWriteWideStringToStream(AStream: TStream; const AValue: WideString);

function dxSkinElementCheckState(AElement: TdxSkinElement; AState: TdxSkinElementState): TdxSkinElementState;
function dxSkinGetElementSize(AElement: TdxSkinElement; AScaleFactor: TdxScaleFactor = nil): TSize;
implementation

uses
  dxTypeHelpers, Variants, StrUtils, dxSVGImage;

{$R dxSkinInfo.res}


type
  TdxSmartGlyphAccess = class(TdxSmartGlyph);
  TdxSmartImageAccess = class(TdxSmartImage);
  TPersistentAccess = class(TPersistent);

  { TdxSkinDefaultColorPalette }

  TdxSkinGeneralColorPalette = class(TdxSkinColorPalette)
  protected
    procedure AssignCore(Source: TPersistent); override;
  end;

  { TdxSkinPartStream }

  TdxSkinPartStream = class(TStream)
  private
    FPosEnd: Longint;
    FPosStart: Longint;
    FSource: TStream;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(ASource: TStream; const APosStart, ASize: LongInt); virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property PosEnd: Longint read FPosEnd;
    property PosStart: Longint read FPosStart;
    property Source: TStream read FSource;
  end;

  { TdxSkinRender }

  TdxSkinRender = class
  public
    class procedure FillRectByGradient(ACanvas: TdxGPCanvas; const R: TRect;
      AColor1, AColor2: TColor; AMode: TdxSkinGradientMode; AAlpha: Byte = 255);
  end;

var
  FRegisteredPropertyTypes: TList;
  FSkinEmptyElement: TdxSkinElement;

procedure SaveTextureToStream(ATexture: TdxSmartImage; AStream: TStream);

  function MustBeConvertedToPNG: Boolean;
  begin
    Result := ATexture.HandleAllocated and
      not (Supports(ATexture, IdxVectorImage)) and
      not (ATexture.ImageDataFormat in [dxImagePng, dxImageGif]);
  end;

begin
  if not ATexture.Empty then
  begin
    if MustBeConvertedToPNG then
      ATexture.SaveToStreamByCodec(AStream, dxImagePng)
    else
      ATexture.SaveToStream(AStream);
  end;
end;

function dxSkinGetAlternateImageAttrsPropertyName(AState: TdxSkinElementState; AImageIndex: Integer = 0): string;
begin
  Result := sdxAlternateImage + IntToStr(AImageIndex + 1) + dxSkinElementStateNames[AState]
end;

function dxSkinReadStringFromStream(AStream: TStream): string;
var
  ALen: Integer;
  AStr: AnsiString;
begin
  AStream.ReadBuffer(ALen, SizeOf(ALen));
  SetLength(AStr, ALen);
  if ALen > 0 then
    AStream.ReadBuffer(AStr[1], ALen);
  Result := dxAnsiStringToString(AStr, CP_UTF8);
end;

function dxSkinReadWideStringFromStream(AStream: TStream): WideString;
var
  ALen: Integer;
begin
  AStream.ReadBuffer(ALen, SizeOf(ALen));
  SetLength(Result, ALen);
  if ALen > 0 then
    AStream.ReadBuffer(Result[1], ALen * SizeOf(WideChar))
  else
    Result := '';
end;

function dxSkinReadColorFromStream(AStream: TStream): TColor;
begin
  AStream.ReadBuffer(Result, SizeOf(Result));
end;

function dxSkinReadIntegerFromStream(AStream: TStream): Integer;
begin
  AStream.ReadBuffer(Result, SizeOf(Result));
end;

procedure dxSkinReadSmartImageFromStream(AStream: TStream; AImage: TdxSmartImage);
var
  APartStream: TdxSkinPartStream;
  ASavedPosition: Int64;
  ASize: Integer;
begin
  AStream.ReadBuffer(ASize, SizeOf(Integer));
  if ASize > 0 then
  begin
    ASavedPosition := AStream.Position;
    try
      APartStream := TdxSkinPartStream.Create(AStream, AStream.Position, ASize);
      try
        AImage.LoadFromStream(APartStream);
      finally
        APartStream.Free;
      end;
    finally
      AStream.Position := ASavedPosition + ASize;
    end;
  end;
end;

procedure dxSkinWriteColorToStream(AStream: TStream; AValue: TColor);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure dxSkinWriteIntegerToStream(AStream: TStream; AValue: Integer);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure dxSkinWriteSmartImageToStream(AStream: TStream; AImage: TdxSmartImage);
var
  AImageStream: TMemoryStream;
  ASize: Integer;
begin
  AImageStream := TMemoryStream.Create;
  try
    SaveTextureToStream(AImage, AImageStream);
    ASize := AImageStream.Size;
    dxSkinWriteIntegerToStream(AStream, ASize);
    if ASize > 0 then
    begin
      AImageStream.Position := 0;
      AStream.WriteBuffer(AImageStream.Memory^, AImageStream.Size);
    end;
  finally
    AImageStream.Free;
  end;
end;

procedure dxSkinWriteStringToStream(AStream: TStream; const AValue: string);
var
  S: AnsiString;
  L: Integer;
begin
  S := dxStringToAnsiString(AValue, CP_UTF8);
  L := Length(S);
  dxSkinWriteIntegerToStream(AStream, L);
  if L > 0 then
    AStream.WriteBuffer(S[1], L);
end;

procedure dxSkinWriteWideStringToStream(AStream: TStream; const AValue: WideString);
var
  ALen: Integer;
begin
  ALen := Length(AValue);
  dxSkinWriteIntegerToStream(AStream, ALen);
  if ALen > 0 then
    AStream.WriteBuffer(AValue[1], ALen * SizeOf(WideChar));
end;

function dxSkinRegisteredPropertyTypes: TList;
begin
  Result := FRegisteredPropertyTypes;
end;

procedure dxSkinInvalidOperation(const AMessage: string);
begin
  raise EdxSkinError.Create(AMessage);
end;

procedure dxSkinCheck(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
     dxSkinInvalidOperation(AMessage);
end;

procedure dxSkinCheckVersion(const AVersion: TdxSkinVersion);
begin
  if AVersion < 1.00 then
    raise EdxSkinError.Create(sdxOldFormat);
end;

function dxSkinCheckSignature(AStream: TStream; out AVersion: TdxSkinVersion): Boolean;
var
  AHeader: TdxSkinHeader;
begin
  Result := (AStream.Read(AHeader, SizeOf(AHeader)) = SizeOf(AHeader)) and
    (AHeader.Signature = dxSkinSignature) and (AHeader.Version >= 1.00);
  if Result then
    AVersion := AHeader.Version;
end;

function dxSkinCheckSkinElement(AElement: TdxSkinElement): TdxSkinElement;
begin
  Result := AElement;
  if Result = nil then
    Result := FSkinEmptyElement;
end;

procedure dxSkinWriteSignature(AStream: TStream);
var
  AHeader: TdxSkinHeader;
begin
  ZeroMemory(@AHeader, SizeOf(AHeader));
  AHeader.Signature := dxSkinSignature;
  AHeader.Version := dxSkinStreamVersion;
  AStream.WriteBuffer(AHeader, SizeOf(AHeader));
end;

procedure dxSkinsCalculatePartsBounds(const R, AMargins: TRect; var AParts);
var
  R1: TRect;
begin
  R1.Top := Min(R.Top + AMargins.Top, R.Bottom);
  R1.Right := Max(R.Right - AMargins.Right, R.Left);
  R1.Bottom := Max(R.Bottom - AMargins.Bottom, R.Top);
  R1.Left := Min(R.Left + AMargins.Left, R.Right);

  TdxSkinElementPartBounds(AParts)[sipCenter] := R1;
  TdxSkinElementPartBounds(AParts)[sipTopLeft] := Rect(R.TopLeft, R1.TopLeft);
  TdxSkinElementPartBounds(AParts)[sipTop] := Rect(R1.Left, R.Top, R1.Right, R1.Top);
  TdxSkinElementPartBounds(AParts)[sipTopRight] := Rect(R1.Right, R.Top, R.Right, R1.Top);
  TdxSkinElementPartBounds(AParts)[sipLeft] := Rect(R.Left, R1.Top, R1.Left, R1.Bottom);
  TdxSkinElementPartBounds(AParts)[sipRight] := Rect(R1.Right, R1.Top, R.Right, R1.Bottom);
  TdxSkinElementPartBounds(AParts)[sipBottomLeft] := Rect(R.Left, R1.Bottom, R1.Left, R.Bottom);
  TdxSkinElementPartBounds(AParts)[sipBottom] := Rect(R1.Left, R1.Bottom, R1.Right, R.Bottom);
  TdxSkinElementPartBounds(AParts)[sipBottomRight] := Rect(R1.Right, R1.Bottom, R.Right, R.Bottom);
end;

procedure dxSkinsCheckMargins(var AMargins: TRect; const R: TRect);

  procedure CheckSide(var S1, S2: Integer; ARectSize: Integer);
  var
    ASize, ADelta: Integer;
  begin
    S1 := Max(S1, 0);
    S2 := Max(S2, 0);
    ASize := S1 + S2;
    ADelta := ASize - ARectSize;
    if ADelta > 0 then
    begin
      Dec(S1, MulDiv(S1, ADelta, ASize));
      Dec(S2, MulDiv(S2, ADelta, ASize));
    end;
  end;

begin
  CheckSide(AMargins.Left, AMargins.Right, cxRectWidth(R));
  CheckSide(AMargins.Top, AMargins.Bottom, cxRectHeight(R));
end;

procedure dxSkinChangeNotify(ASender: TObject; ANotifyEvent: TdxSkinChangeNotify; AChanges: TdxSkinChanges);
begin
  if Assigned(ANotifyEvent) and (AChanges <> []) then
    ANotifyEvent(ASender, AChanges);
end;

function dxSkinElementCheckState(AElement: TdxSkinElement; AState: TdxSkinElementState): TdxSkinElementState;
begin
  Result := AState;
  if not (AState in AElement.Image.States) then
  begin
    case AState of
      esHotCheck, esChecked, esCheckPressed, esDroppedDown:
        Result := esPressed;
      esActiveDisabled, esActive:
        Result := esHot;
    end;
  end;
end;

function dxSkinGetElementSize(AElement: TdxSkinElement; AScaleFactor: TdxScaleFactor = nil): TSize;
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;
  if AElement <> nil then
    Result := AScaleFactor.Apply(cxSizeMax(AElement.Size, AElement.MinSize.Size))
  else
    Result := cxNullSize;
end;

{ TdxSkinCustomObjectList }

constructor TdxSkinCustomObjectList.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

constructor TdxSkinCustomObjectList.CreateEx(AOwner: TPersistent; AChangeHandler: TdxSkinChangeNotify);
begin
  Create(AOwner);
  OnChange := AChangeHandler;
end;

procedure TdxSkinCustomObjectList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxSkinCustomObjectList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed(FChanges);
end;

procedure TdxSkinCustomObjectList.Clear;
begin
  BeginUpdate;
  try
    inherited Clear;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinCustomObjectList.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
  Changed([scStruct]);
end;

function TdxSkinCustomObjectList.Find(const AName: string; var AObject): Boolean;
var
  L, H, I, C: Integer;
begin
  Sort;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) div 2;
    C := AnsiCompareStr(TdxSkinCustomObject(List[I]).Name, AName);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        TdxSkinCustomObject(AObject) := TdxSkinCustomObject(List[I]);
        Exit(True);
      end
    end;
  end;
  Result := False;
end;

function TdxSkinCustomObjectList.IndexOf(const AName: string): Integer;
var
  AObject: TObject;
begin
  if Find(AName, AObject) then
    Result := IndexOf(AObject)
  else
    Result := -1;
end;

procedure TdxSkinCustomObjectList.Sort(ASortChildren: Boolean = False);
begin
  if not FSorted then
  begin
    inherited Sort(TListSortCompare(@CompareByName));
    FSorted := True;
  end;
  if ASortChildren then
    SortChildren;
end;

procedure TdxSkinCustomObjectList.Changed(AChanges: TdxSkinChanges);
begin
  if scStruct in AChanges then
  begin
    FSorted := False;
  end;

  FChanges := FChanges + AChanges;
  if FUpdateCount = 0 then
  begin
    dxSkinChangeNotify(Self, OnChange, FChanges);
    FChanges := [];
  end;
end;

procedure TdxSkinCustomObjectList.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
var
  ACount: Integer;
begin
  BeginUpdate;
  try
    ACount := dxSkinReadIntegerFromStream(AStream);
    while ACount > 0 do
    begin
      DataReadItem(AStream, AVersion);
      Dec(ACount);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinCustomObjectList.DataWrite(AStream: TStream);
var
  I: Integer;
begin
  WriteIntegerToStream(AStream, Count);
  for I := 0 to Count - 1 do
    DataWriteItem(AStream, TdxSkinCustomObject(List[I]));
end;

procedure TdxSkinCustomObjectList.SubItemChanged(ASender: TObject; AChanges: TdxSkinChanges);
begin
  Changed(AChanges);
end;

procedure TdxSkinCustomObjectList.FlushCache;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxSkinCustomObject(List[I]).FlushCache;
end;

procedure TdxSkinCustomObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  Changed([scStruct]);
end;

procedure TdxSkinCustomObjectList.SortChildren;
begin
  // do nothing
end;

class function TdxSkinCustomObjectList.CompareByName(AItem1, AItem2: TdxSkinCustomObject): Integer;
begin
  Result := AnsiCompareStr(AItem1.Name, AItem2.Name);
end;

{ TdxSkinControlGroups }

function TdxSkinControlGroups.Add(const AName: string): TdxSkinControlGroup;
begin
  Result := TdxSkinControlGroup.Create(Owner, AName);
  Result.OnChange := SubItemChanged;
  inherited Add(Result);
end;

procedure TdxSkinControlGroups.Assign(ASource: TdxSkinControlGroups);
var
  I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    for I := 0 to ASource.Count - 1 do
      Add(ASource[I].Name).Assign(ASource[I]);
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinControlGroups.Dormant;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].Dormant;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinControlGroups.DataReadItem(AStream: TStream; const AVersion: TdxSkinVersion);
begin
  Add(dxSkinReadStringFromStream(AStream)).DataRead(AStream, AVersion);
end;

procedure TdxSkinControlGroups.DataWriteItem(AStream: TStream; AItem: TdxSkinCustomObject);
begin
  dxSkinWriteStringToStream(AStream, AItem.Name);
  AItem.DataWrite(AStream);
end;

procedure TdxSkinControlGroups.SortChildren;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Sort(True);
end;

function TdxSkinControlGroups.GetItem(Index: Integer): TdxSkinControlGroup;
begin
  Result := TdxSkinControlGroup(inherited Items[Index]);
end;

{ TdxSkinElements }

function TdxSkinElements.Add(const AName: string; AClass: TdxSkinElementClass): TdxSkinElement;
begin
  Result := AClass.Create(Owner, AName);
  Result.OnChange := SubItemChanged;
  inherited Add(Result);
end;

procedure TdxSkinElements.Assign(ASource: TdxSkinElements);
var
  AElement: TdxSkinElement;
  I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    for I := 0 to ASource.Count - 1 do
    begin
      AElement := ASource[I];
      Add(AElement.Name, TdxSkinElementClass(AElement.ClassType)).Assign(AElement);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinElements.Dormant;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].Dormant;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinElements.DataReadItem(AStream: TStream; const AVersion: TdxSkinVersion);
begin
  Add(dxSkinReadStringFromStream(AStream), TdxSkinElement).DataRead(AStream, AVersion);
end;

procedure TdxSkinElements.DataWriteItem(AStream: TStream; AItem: TdxSkinCustomObject);
begin
  dxSkinWriteStringToStream(AStream, AItem.Name);
  AItem.DataWrite(AStream);
end;

procedure TdxSkinElements.SortChildren;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Sort(True);
end;

function TdxSkinElements.GetItem(Index: Integer): TdxSkinElement;
begin
  Result := TdxSkinElement(inherited Items[Index]);
end;

{ TdxSkinColorValue }

constructor TdxSkinColorValue.Create(AOwner: TdxSkinCustomObject; ADefaultValue: TColor = clDefault);
begin
  FOwner := AOwner;
  FDefaultValue := ADefaultValue;
  FValue := FDefaultValue;
end;

procedure TdxSkinColorValue.Assign(const ASource: TdxSkinColorValue);
begin
  FValue := ASource.FValue;
  FValueReference := ASource.FValueReference;
  Changed;
end;

procedure TdxSkinColorValue.Changed;
begin
  FlushCache;
  if FOwner <> nil then
    FOwner.Changed([scContent]);
end;

procedure TdxSkinColorValue.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
begin
  FValue := dxSkinReadColorFromStream(AStream);
  if AVersion >= 1.15 then
    FValueReference := dxSkinReadStringFromStream(AStream);
end;

procedure TdxSkinColorValue.DataWrite(AStream: TStream);
begin
  dxSkinWriteColorToStream(AStream, FValue);
  dxSkinWriteStringToStream(AStream, FValueReference);
end;

function TdxSkinColorValue.Equals(const ASource: TdxSkinColorValue): Boolean;
begin
  Result := (ASource.FValue = FValue) and (ASource.FValueReference = FValueReference);
end;

function TdxSkinColorValue.GetColorPalette(out APalette: IdxSkinColorPalette): Boolean;
var
  AOwner: TPersistent;
begin
  AOwner := FOwner;
  while AOwner <> nil do
  begin
    if Supports(AOwner, IdxSkinColorPalette, APalette) then
      Exit(True);
    AOwner := TPersistentAccess(AOwner).GetOwner;
  end;
  Result := False;
end;

procedure TdxSkinColorValue.FlushCache;
begin
  if ValueReference <> '' then
    FValue := clUnassigned;
end;

procedure TdxSkinColorValue.ResetToDefaults;
begin
  Value := FDefaultValue;
end;

function TdxSkinColorValue.GetValue: TColor;
var
  APalette: IdxSkinColorPalette;
begin
  if FValue = clUnassigned then
  begin
    if not ((ValueReference <> '') and GetColorPalette(APalette) and APalette.GetColor(ValueReference, FValue)) then
      FValue := FDefaultValue;
  end;
  Result := FValue;
end;

procedure TdxSkinColorValue.SetValue(const AValue: TColor);
begin
  if (FValue <> AValue) or (ValueReference <> '') then
  begin
    FValue := AValue;
    FValueReference := '';
    Changed;
  end;
end;

procedure TdxSkinColorValue.SetValueReference(const AValue: string);
begin
  if FValueReference <> AValue then
  begin
    if AValue = '' then
    begin
      FValue := Value;
      FValueReference := '';
    end
    else
      FValueReference := AValue;

    Changed;
  end;
end;

{ TdxSkinProperties }

function TdxSkinProperties.Add(const AName: string; AClass: TdxSkinPropertyClass): TdxSkinProperty;
begin
  Result := AClass.Create(Owner, AName);
  Result.OnChange := SubItemChanged;
  inherited Add(Result);
end;

procedure TdxSkinProperties.Assign(ASource: TdxSkinProperties);
var
  AProperty: TdxSkinProperty;
  I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    for I := 0 to ASource.Count - 1 do
    begin
      AProperty := ASource.Items[I];
      Add(AProperty.Name, TdxSkinPropertyClass(AProperty.ClassType)).Assign(AProperty);
    end;
  finally
    EndUpdate;
  end;
end;

function TdxSkinProperties.Compare(AProperties: TdxSkinProperties): Boolean;
var
  I: Integer;
begin
  Result := AProperties.Count = Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := AProperties[I].Compare(Items[I]);
      if not Result then Break;
    end;
end;

procedure TdxSkinProperties.DataReadItem(AStream: TStream; const AVersion: TdxSkinVersion);
var
  AClass: TClass;
begin
  AClass := FindClass(dxSkinReadStringFromStream(AStream));
  Add(dxSkinReadStringFromStream(AStream), TdxSkinPropertyClass(AClass)).DataRead(AStream, AVersion);
end;

procedure TdxSkinProperties.DataWriteItem(AStream: TStream; AItem: TdxSkinCustomObject);
begin
  dxSkinWriteStringToStream(AStream, AItem.ClassName);
  dxSkinWriteStringToStream(AStream, AItem.Name);
  AItem.DataWrite(AStream);
end;

function TdxSkinProperties.GetItem(Index: Integer): TdxSkinProperty;
begin
  Result := TdxSkinProperty(List[Index]);
end;

{ TdxSkinColors }

function TdxSkinColors.Add(const AName: string; AValue: TColor): TdxSkinColor;
begin
  BeginUpdate;
  try
    Result := TdxSkinColor(inherited Add(AName, TdxSkinColor));
    Result.Value := AValue;
  finally
    EndUpdate;
  end;
end;

function TdxSkinColors.GetItem(Index: Integer): TdxSkinColor;
begin
  Result := TdxSkinColor(List[Index]);
end;

{ TdxSkinPersistent }

constructor TdxSkinPersistent.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create(AOwner, AName);
  FProperties := TdxSkinProperties.CreateEx(Self, SubItemChanged);
end;

destructor TdxSkinPersistent.Destroy;
begin
  FProperties.Clear;
  FreeAndNil(FProperties);
  inherited Destroy;
end;

function TdxSkinPersistent.AddProperty(const AName: string; APropertyClass: TdxSkinPropertyClass): TdxSkinProperty;
begin
  Result := FProperties.Add(AName, APropertyClass);
end;

function TdxSkinPersistent.AddPropertyBool(const AName: string; AValue: Boolean): TdxSkinBooleanProperty;
begin
  BeginUpdate;
  try
    Result := TdxSkinBooleanProperty(AddProperty(AName, TdxSkinBooleanProperty));
    Result.Value := AValue;
  finally
    EndUpdate;
  end;
end;

function TdxSkinPersistent.AddPropertyColor(const AName: string; AValue: TColor): TdxSkinColor;
begin
  BeginUpdate;
  try
    Result := TdxSkinColor(AddProperty(AName, TdxSkinColor));
    Result.Value := AValue;
  finally
    EndUpdate;
  end;
end;

function TdxSkinPersistent.AddPropertyInteger(
  const AName: string; AValue: Integer): TdxSkinIntegerProperty;
begin
  BeginUpdate;
  try
    Result := TdxSkinIntegerProperty(AddProperty(AName, TdxSkinIntegerProperty));
    Result.Value := AValue;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinPersistent.Clear;
begin
  FProperties.Clear;
end;

procedure TdxSkinPersistent.DeleteProperty(const AProperty: TdxSkinProperty);
begin
  FProperties.FreeAndRemove(AProperty);
end;

procedure TdxSkinPersistent.DeleteProperty(const APropertyName: string);
begin
  DeleteProperty(GetPropertyByName(APropertyName));
end;

procedure TdxSkinPersistent.ExchangeProperty(AIndex1, AIndex2: Integer);
begin
  FProperties.Exchange(AIndex1, AIndex2);
end;

procedure TdxSkinPersistent.Sort(ASortChildren: Boolean = False);
begin
  FProperties.Sort(ASortChildren);
end;

procedure TdxSkinPersistent.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinPersistent then
    FProperties.Assign(TdxSkinPersistent(Source).FProperties)
end;

procedure TdxSkinPersistent.Changed(AChanges: TdxSkinChanges);
begin
  Modified := True;
  inherited Changed(AChanges);
end;

procedure TdxSkinPersistent.FlushCache;
begin
  inherited FlushCache;
  FProperties.FlushCache;
end;

procedure TdxSkinPersistent.SubItemChanged(ASender: TObject; AChanges: TdxSkinChanges);
begin
  Changed(AChanges);
end;

function TdxSkinPersistent.GetPropertyByName(const AName: string): TdxSkinProperty;
begin
  if not GetPropertyByName(AName, Result) then
    Result := nil;
end;

function TdxSkinPersistent.GetPropertyByName(const AName: string; out AProperty: TdxSkinProperty): Boolean;
begin
  Result := FProperties.Find(AName, AProperty);
end;

function TdxSkinPersistent.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TdxSkinPersistent.GetProperty(Index: Integer): TdxSkinProperty;
begin
  Result := FProperties[Index];
end;

{ TdxSkin }

constructor TdxSkin.Create(const AName: string; ALoadOnCreate: Boolean; hInst: THandle);
begin
  inherited Create(nil, AName);
  FListeners := TInterfaceList.Create;
  FDetails := TdxSkinDetails.Create;
  FDetails.Name := AName;
  FDetails.OnChange := HandlerDetailsChanged;
  FColors := TdxSkinColors.CreateEx(Self, SubItemChanged);
  FGroups := TdxSkinControlGroups.CreateEx(Self, SubItemChanged);
  FColorPalette := TdxSkinGeneralColorPalette.Create(Self, sdxDefaultColorPaletteName);
  FColorPalettes := TdxSkinColorPalettes.Create(Self, '');
  FColorPalettes.OnChange := HandlerColorPalettesChanged;
  if ALoadOnCreate then
    LoadFromResource(hInst);
end;

destructor TdxSkin.Destroy;
begin
  FDestroying := True;

  FreeAndNil(FColorPalettes);
  FreeAndNil(FColorPalette);
  FreeAndNil(FDetails);
  FreeAndNil(FListeners);
  FreeAndNil(FColors);
  FreeAndNil(FGroups);
  inherited Destroy;
end;

function TdxSkin.AddColor(const AName: string; const AColor: TColor = clDefault): TdxSkinColor;
begin
  Result := FColors.Add(AName, AColor);
end;

function TdxSkin.AddGroup(const AName: string): TdxSkinControlGroup;
begin
  Result := FGroups.Add(AName);
end;

procedure TdxSkin.AddListener(AListener: IUnknown);
begin
  Listeners.Add(AListener);
end;

procedure TdxSkin.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkin then
  begin
    Details.Assign(TdxSkin(Source).Details);
    FColors.Assign(TdxSkin(Source).FColors);
    FGroups.Assign(TdxSkin(Source).FGroups);
    FColorPalettes.Assign(TdxSkin(Source).FColorPalettes);
    FColorPalette.Assign(TdxSkin(Source).FColorPalette);
  end;
  inherited AssignCore(Source);
end;

procedure TdxSkin.Clear;
begin
  inherited Clear;
  Details.Clear;
  FColorPalettes.Clear;
  FGroups.Clear;
  FColors.Clear;
end;

procedure TdxSkin.ClearModified;
var
  I: Integer;
begin
  FModified := False;
  for I := 0 to GroupCount - 1 do
    Groups[I].ClearModified;
end;

function TdxSkin.Clone(const AName: string): TdxSkin;
begin
  Result := TdxSkinClass(ClassType).Create(Name, False, 0);
  Result.Assign(Self);
end;

function TdxSkin.GetColorByName(const AName: string): TdxSkinColor;
begin
  if not FColors.Find(AName, Result) then
    Result := nil;
end;

procedure TdxSkin.HandlerColorPalettesChanged(Sender: TObject; AChanges: TdxSkinChanges);
begin
  if FDestroying then Exit;

  BeginUpdate;
  try
    SelectColorPalette(ActiveColorPaletteName);
    Changed(AChanges);
  finally
    EndUpdate;
  end;
end;

procedure TdxSkin.HandlerDetailsChanged(Sender: TObject);
begin
  Changed([scDetails]);
end;

procedure TdxSkin.FlushCache;
begin
  inherited FlushCache;
  FColorPalette.FlushCache;
  FColors.FlushCache;
  FGroups.FlushCache;
end;

procedure TdxSkin.DoChanged(AChanges: TdxSkinChanges);
begin
  if not FDestroying then
    FlushCache;
  NotifyListeners(AChanges);
  inherited DoChanged(AChanges);
end;

procedure TdxSkin.DeleteGroup(const AGroup: TdxSkinControlGroup);
begin
  FGroups.FreeAndRemove(AGroup);
end;

procedure TdxSkin.DeleteProperty(const AProperty: TdxSkinProperty);
begin
  inherited DeleteProperty(AProperty);
  FColors.FreeAndRemove(AProperty);
end;

procedure TdxSkin.Dormant;
begin
  FGroups.Dormant;
end;

procedure TdxSkin.ExchangeColors(AIndex1, AIndex2: Integer);
begin
  FColors.Exchange(AIndex1, AIndex2);
end;

procedure TdxSkin.ExchangeGroups(AIndex1, AIndex2: Integer);
begin
  FGroups.Exchange(AIndex1, AIndex2);
end;

function TdxSkin.GetGroupByName(const AName: string): TdxSkinControlGroup;
begin
  if not GetGroupByName(AName, Result) then
    Result := nil;
end;

function TdxSkin.GetGroupByName(const AName: string; out AGroup: TdxSkinControlGroup): Boolean;
begin
  Result := FGroups.Find(AName, AGroup);
end;

procedure TdxSkin.LoadFromBinaryFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromBinaryStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSkin.LoadFromBinaryStream(AStream: TStream);
var
  AReader: TdxSkinBinaryReader;
begin
  AReader := TdxSkinBinaryReader.Create(AStream);
  try
    AReader.LoadSkin(Self, 0);
  finally
    AReader.Free;
  end;
end;

procedure TdxSkin.LoadFromStream(AStream: TStream);
var
  AVersion: TdxSkinVersion;
begin
  if not CheckGdiPlus then
    Exit;
  if not dxSkinCheckSignature(AStream, AVersion) then
    raise EdxSkinError.Create(sdxSkinInvalidStreamFormat);

  BeginUpdate;
  try
    Clear;
    Details.DataRead(AStream, AVersion);
    FColors.DataRead(AStream, AVersion);
    FProperties.DataRead(AStream, AVersion);
    FGroups.DataRead(AStream, AVersion);
    if AVersion >= 1.15 then
      FColorPalettes.DataRead(AStream, AVersion);
  finally
    EndUpdate;
  end;
end;

procedure TdxSkin.LoadFromFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free
  end;
end;

procedure TdxSkin.RemoveListener(AListener: IUnknown);
begin
  Listeners.Remove(AListener);
end;

procedure TdxSkin.SaveToFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free
  end;
end;

procedure TdxSkin.SaveToStream(AStream: TStream);
begin
  dxSkinWriteSignature(AStream);
  Details.DataWrite(AStream);
  FColors.DataWrite(AStream);
  FProperties.DataWrite(AStream);
  FGroups.DataWrite(AStream);
  FColorPalettes.DataWrite(AStream);
end;

procedure TdxSkin.SaveToBinaryFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToBinaryStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSkin.SaveToBinaryStream(AStream: TStream);
begin
  with TdxSkinBinaryWriter.Create(AStream) do
  try
    AddSkin(Self);
  finally
    Free;
  end;
end;

function TdxSkin.GetGlyphColorPalette(AState: TdxSkinElementState): IdxColorPalette;
var
  APalette: TdxSkinColorPalette;
  AProperty: TdxSkinProperty;
begin
  if GetPropertyByName(sdxDefaultGlyphColorPalettes, AProperty) and (AProperty is TdxSkinColorPalettes) then
  begin
    if TdxSkinColorPalettes(AProperty).Find(dxSkinElementStateNames[AState], APalette) then
      Exit(APalette);
    if (AState <> esNormal) and TdxSkinColorPalettes(AProperty).Find(dxSkinElementStateNames[esNormal], APalette) then
      Exit(APalette);
  end;
  Result := ActiveColorPalette;
end;

procedure TdxSkin.NotifyListener(AChanges: TdxSkinChanges; const ACustomListener: IUnknown);
var
  AListener: IdxSkinChangeListener;
  AListener2: IdxSkinChangeListener2;
begin
  if Supports(ACustomListener, IdxSkinChangeListener2, AListener2) then
    AListener2.SkinChanged(Self, AChanges)
  else
    if Supports(ACustomListener, IdxSkinChangeListener, AListener) then
      AListener.SkinChanged(Self);
end;

procedure TdxSkin.NotifyListeners(AChanges: TdxSkinChanges);
var
  I: Integer;
begin
  if not FDestroying then
  begin
    BeginUpdate;
    try
      for I := 0 to Listeners.Count - 1 do
        NotifyListener(AChanges, Listeners[I]);
    finally
      CancelUpdate;
    end;
  end;
end;

procedure TdxSkin.LoadFromResource(hInst: THandle);
var
  AStream: TStream;
begin
  AStream := TResourceStream.Create(hInst, Name, PChar(sdxResourceType));
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSkin.SelectColorPalette(const Value: string);
var
  APalette: TdxSkinColorPalette;
begin
  if not ColorPalettes.Find(Value, APalette) then
  begin
    if not ColorPalettes.Find(sdxDefaultColorPaletteName, APalette) then
    begin
      if ColorPalettes.Count > 0 then
        APalette := ColorPalettes[0]
      else
        APalette := nil;
    end;
  end;

  if APalette <> nil then
    FColorPalette.Assign(APalette)
  else
  begin
    FColorPalette.Clear;
    FColorPalette.Name := sdxDefaultColorPaletteName;
  end;

  Changed([scContent]);
end;

function TdxSkin.PaletteGetColor(const AKey: string; out AColor: TColor): Boolean;
var
  AProperty: TdxSkinProperty;
begin
  Result := ActiveColorPalette.FProperties.Find(AKey, AProperty);
  if Result then
    AColor := TdxSkinColor(AProperty).Value;
end;

procedure TdxSkin.Sort(ASortChildren: Boolean = False);
begin
  inherited Sort(ASortChildren);
  FColorPalettes.Sort(ASortChildren);
  FGroups.Sort(ASortChildren);
  FColors.Sort(ASortChildren);
end;

function TdxSkin.GetName: string;
begin
  Result := Details.Name;
end;

function TdxSkin.GetColor(Index: Integer): TdxSkinColor;
begin
  Result := FColors[Index];
end;

function TdxSkin.GetColorCount: Integer;
begin
  Result := FColors.Count;
end;

function TdxSkin.GetActiveColorPaletteName: string;
begin
  Result := FColorPalette.Name;
end;

function TdxSkin.GetDisplayName: string;
begin
  Result := Details.DisplayName;
  if Result = '' then
    Result := Name;
end;

function TdxSkin.GetGroup(Index: Integer): TdxSkinControlGroup;
begin
  Result := FGroups[Index];
end;

function TdxSkin.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;

function TdxSkin.GetHasMissingElements: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GroupCount - 1 do
  begin
    Result := Groups[I].HasMissingElements;
    if Result then Break;
  end;
end;

procedure TdxSkin.SetActiveColorPaletteName(const Value: string);
begin
  if ActiveColorPaletteName <> Value then
    SelectColorPalette(Value);
end;

procedure TdxSkin.SetName(const Value: string);
begin
  Details.Name := Value;
end;

{ TdxSkinImage }

constructor TdxSkinImage.Create(AOwner: TdxSkinElement);
begin
  inherited Create;
  FOwner := AOwner;
  FStates := [esNormal];
  FTextureIsVector := bDefault;
  FTexture := CreateTexture;
  FTexture.OnChange := HandlerTextureChanged;
  FMargins := TcxMargin.Create(Self);
  FMargins.OnChange := HandlerMarginsChanged;
  FGradientBeginColor := TdxSkinColorValue.Create(AOwner, clNone);
  FGradientEndColor := TdxSkinColorValue.Create(AOwner, clNone);
  FColorPalettes := TdxSkinColorPalettes.Create(Self, '');
  FColorPalettes.FDefaultColorClass := TdxSkinAlphaColor;
  FColorPalettes.OnChange := HandlerColorPaletteChanged;
  FGradient := gmHorizontal;
  FImageCount := 1;
  FIsDirty := True;
end;

destructor TdxSkinImage.Destroy;
begin
  FreeAndNil(FColorPalettes);
  FreeAndNil(FMargins);
  FreeAndNil(FTexture);
  inherited Destroy;
end;

procedure TdxSkinImage.Assign(Source: TPersistent);
begin
  if Source is TdxSkinImage then
  begin
    if TdxSkinImage(Source).Empty then
      Clear
    else
    begin
      Texture.Assign(TdxSkinImage(Source).Texture);
      FSourceName := TdxSkinImage(Source).SourceName;
    end;
    FColorPalettes.Assign(TdxSkinImage(Source).FColorPalettes);
    FGradientEndColor.Assign(TdxSkinImage(Source).FGradientEndColor);
    FGradientBeginColor.Assign(TdxSkinImage(Source).FGradientBeginColor);
    Gradient := TdxSkinImage(Source).Gradient;
    ImageCount := TdxSkinImage(Source).ImageCount;
    ImageLayout := TdxSkinImage(Source).ImageLayout;
    Margins.Assign(TdxSkinImage(Source).Margins);
    FScaleMargins := TdxSkinImage(Source).ScaleMargins;
    Stretch := TdxSkinImage(Source).Stretch;
    States := TdxSkinImage(Source).States;
    InterpolationMode := TdxSkinImage(Source).InterpolationMode;
  end;
end;

procedure TdxSkinImage.Clear;
begin
  if (FSourceName <> '') or not Texture.Empty then
  begin
    FSourceName := '';
    Texture.Clear;
    Changed;
  end;
end;

procedure TdxSkinImage.GetBitmap(AImageIndex: Integer;
  AState: TdxSkinElementState; ABitmap: TBitmap; ABackgroundColor: TColor = clNone);
begin
  ABitmap.FreeImage;
  ABitmap.SetSize(Size.cx, Size.cy);
  if ABackgroundColor <> clNone then
  begin
    if ABackgroundColor <> clDefault then
      ABitmap.Canvas.Brush.Color := ABackgroundColor;
    ABitmap.Canvas.FillRect(cxRect(Size));
  end;
  Draw(ABitmap.Canvas.Handle, cxRect(Size), AImageIndex, AState);
end;

function TdxSkinImage.GetPalette(AState: TdxSkinElementState; ACanUseGlobalPalette: Boolean = True): IdxColorPalette;
var
  APalette: TdxSkinColorPalette;
begin
  if ColorPalettes.Find(dxSkinElementStateNames[AState], APalette) then
    Result := APalette
  else
    if AState <> esNormal then
      Result := GetPalette(esNormal, ACanUseGlobalPalette)
    else
      if (Owner <> nil) and ACanUseGlobalPalette then
        Result := Owner.Group.Skin
      else
        Result := nil;
end;

procedure TdxSkinImage.LoadFromFile(const AFileName: string);
var
  AStream: TStream;
begin
  FSourceName := AFileName;
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSkinImage.LoadFromResource(AInstance: THandle; const AName: string; AType: PChar);
var
  AStream: TStream;
begin
  AStream := TResourceStream.Create(AInstance, AName, AType);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxSkinImage.LoadFromStream(AStream: TStream);
begin
  Texture.LoadFromStream(AStream);
end;

procedure TdxSkinImage.SaveToFile(const AFileName: string);
var
  AStream: TStream;
begin
  if not Empty then
  begin
    FSourceName := ChangeFileExt(AFileName, GetFileExt);
    AStream := TFileStream.Create(FSourceName, fmCreate);
    try
      SaveToStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxSkinImage.SaveToStream(AStream: TStream);
begin
  SaveTextureToStream(Texture, AStream);
end;

procedure TdxSkinImage.SetStateMapping(ATargetStateOrder: array of TdxSkinElementState);
var
  ABitmap: TBitmap;
  AImageIndex, AStateIndex: Integer;
  ANewStates: TdxSkinElementStates;
  ATextureSize, AOffset: TSize;
  R: TRect;
begin
  if Texture.Empty then Exit;

  AOffset := Size;
  if ImageLayout = ilHorizontal then
    AOffset.cy := 0
  else
    AOffset.cx := 0;

  ATextureSize := cxSize(Texture.Width, Texture.Height);
  if ImageLayout = ilHorizontal then
    ATextureSize.cx := Size.cx * Length(ATargetStateOrder)
  else
    ATextureSize.cy := Size.cy * Length(ATargetStateOrder);

  ABitmap := TcxBitmap32.CreateSize(ATextureSize.cx, ATextureSize.cy, True);
  try
    ANewStates := [];
    R := cxRect(Size);

    ABitmap.Canvas.Lock;
    try
      for AImageIndex := 0 to ImageCount - 1 do
        for AStateIndex := Low(ATargetStateOrder) to High(ATargetStateOrder) do
        begin
          if ATargetStateOrder[AStateIndex] in States then
            Texture.StretchDraw(ABitmap.Canvas.Handle, R, StateBounds[AImageIndex, ATargetStateOrder[AStateIndex]]);
          Include(ANewStates, ATargetStateOrder[AStateIndex]);
          OffsetRect(R, AOffset.cx, AOffset.cy);
        end;
    finally
      ABitmap.Canvas.Unlock;
    end;

    Texture.SetBitmap(ABitmap);
    States := ANewStates;
  finally
    ABitmap.Free;
  end;
end;

procedure TdxSkinImage.Changed;
begin
  IsDirty := True;
  dxCallNotify(OnChange, Self);
end;

procedure TdxSkinImage.CheckCalculatePartsBounds(const ASize: TSize);

  procedure CalculateStateBounds;
  var
    AOffset: TPoint;
    ARect: TRect;
    AState: TdxSkinElementState;
  begin
    ARect := cxRect(ASize);

    AOffset := cxNullPoint;
    if not IsVectorTexture then
    begin
      if ImageLayout = ilHorizontal then
        AOffset.X := ASize.cx
      else
        AOffset.Y := ASize.cy;
    end;

    for AState := Low(TdxSkinElementState) to High(TdxSkinElementState) do
    begin
      if AState in States then
      begin
        FStateBounds[AState] := ARect;
        ARect := cxRectOffset(ARect, AOffset);
      end
      else
        FStateBounds[AState] := cxNullRect;
    end;
  end;

var
  APart: TdxSkinImagePart;
begin
  if not cxSizeIsEqual(ASize, FPartsBoundsSourceSize) then
  begin
    FPartsBoundsSourceSize := ASize;
    dxSkinsCalculatePartsBounds(cxRect(ASize), Margins.Margin, FPartsBounds);
    for APart := Low(TdxSkinImagePart) to High(TdxSkinImagePart) do
      FPartsVisibility[APart] := not IsRectEmpty(FPartsBounds[APart]);
    CalculateStateBounds;
  end;
end;

procedure TdxSkinImage.CheckInfo;
begin
  if IsDirty then
  begin
    IsDirty := False;
    InitializeInfo;
  end;
end;

procedure TdxSkinImage.CheckState(var AState: TdxSkinElementState; var AImageIndex: Integer);

  function GetFirstState: TdxSkinElementState;
  begin
    for Result := Low(TdxSkinElementState) to High(TdxSkinElementState) do
    begin
      if Result in FStates then
        Break;
    end;
  end;

begin
  AImageIndex := Min(AImageIndex, ImageCount - 1);
  if not (AState in FStates) then
  begin
    if FStates <> [] then
      AState := GetFirstState;
  end;
end;

function TdxSkinImage.Compare(AImage: TdxSkinImage): Boolean;
begin
  Result := (AImage.ImageLayout = ImageLayout) and (AImage.Empty = Empty) and
    (AImage.States = States) and (AImage.Gradient = Gradient) and
    (AImage.Stretch = Stretch) and (AImage.Size.cx = Size.cx) and (AImage.Size.cy = Size.cy) and
    AImage.FColorPalettes.Compare(FColorPalettes) and
    AImage.Margins.IsEqual(Margins) and Texture.Compare(AImage.Texture) and
    AImage.FGradientBeginColor.Equals(FGradientBeginColor) and
    AImage.FGradientEndColor.Equals(FGradientEndColor);
end;

procedure TdxSkinImage.Dormant;
begin
  Texture.Dormant;
end;

procedure TdxSkinImage.Draw(DC: HDC; const ARect: TRect; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);
begin
  Draw(DC, ARect, dxSystemScaleFactor, AImageIndex, AState);
end;

procedure TdxSkinImage.Draw(DC: HDC; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; AImageIndex: Integer; AState: TdxSkinElementState);
begin
  dxGPPaintCanvas.BeginPaint(DC, ARect);
  try
    DrawEx(dxGPPaintCanvas, ARect, AScaleFactor, AImageIndex, AState);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxSkinImage.DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect;
  AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);
begin
  DrawEx(ACanvas, ARect, dxSystemScaleFactor, AImageIndex, AState);
end;

procedure TdxSkinImage.DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; AImageIndex: Integer; AState: TdxSkinElementState);

  procedure DoDrawPart(const ATargetRect, ASourceRect: TRect; APart: TdxSkinImagePart; AImageHandle: TdxGPImageHandle);
  begin
    if (APart = sipCenter) and IsGradientParamsAssigned then
      TdxSkinRender.FillRectByGradient(ACanvas, ATargetRect, GradientBeginColor, GradientEndColor, Gradient)
    else
      if Stretch = smStretch then
        dxGpDrawImage(ACanvas.Handle, ATargetRect, ASourceRect, AImageHandle.Handle)
      else
        dxGpTilePartEx(ACanvas.Handle, ATargetRect, ASourceRect, AImageHandle.Handle);
  end;

  function GetActualMargins(const AMargins: TRect): TRect;
  begin
    if ScaleMargins then
      Result := AScaleFactor.Apply(AMargins, GetSourceDPI, dxDefaultDPI)
    else
      Result := AMargins;
  end;

  function GetScaledSize: TSize;
  begin
    Result := AScaleFactor.Apply(Size, GetSourceDPI, dxDefaultDPI);
  end;

  procedure DrawStretched(const AImageHandle: TdxGPImageHandle; const ASourceOrigin: TPoint; AMargins: TRect);
  var
    ADestParts: TdxSkinElementPartBounds;
    APart: TdxSkinImagePart;
  begin
    dxSkinsCheckMargins(AMargins, ARect);
    if not cxRectIsNull(AMargins) then
    begin
      dxSkinsCalculatePartsBounds(ARect, GetActualMargins(AMargins), ADestParts);
      for APart := Low(TdxSkinImagePart) to High(TdxSkinImagePart) do
      begin
        if PartVisible[APart] then
          DoDrawPart(ADestParts[APart], cxRectOffset(FPartsBounds[APart], ASourceOrigin), APart, AImageHandle);
      end;
    end
    else
      DoDrawPart(ARect, cxRectOffset(FPartsBounds[sipCenter], ASourceOrigin), sipCenter, AImageHandle);
  end;

  function PrepareImageHandle: TdxGPImageHandle;
  var
    AHandle: TObject;
    ARasterSize: TSize;
  begin
    AHandle := TdxSmartImageAccess(Texture).HandleAsObject;
    if IsVectorTexture then
    begin
      ARasterSize := GetScaledSize;
      if Stretch = smStretch then
      begin
        if cxRectIsNull(Margins.Margin) then
          ARasterSize := cxSizeProportionalStretch(cxSize(ARect), Size);
      end;
      CheckCalculatePartsBounds(ARasterSize);
      TdxSVGImageHandle(AHandle).InterpolationMode := InterpolationMode;
      Result := TdxSVGImageHandle(AHandle).GetRasterizedImage(ARasterSize, GetPalette(AState));
    end
    else
      Result := TdxGPImageHandle(AHandle);
  end;

var
  AImageHandle: TdxGPImageHandle;
  APrevInterpolationMode: TdxGPInterpolationMode;
  APrevPixelOffsetMode: TdxGpPixelOffsetMode;
begin
  if Empty then
  begin
    TdxSkinRender.FillRectByGradient(ACanvas,
      cxRectContent(ARect, GetActualMargins(Margins.Margin)),
      GradientBeginColor, GradientEndColor, Gradient);
    Exit;
  end;

  CheckInfo;
  CheckState(AState, AImageIndex);

  APrevInterpolationMode := ACanvas.InterpolationMode;
  try
    if InterpolationMode <> imDefault then
      ACanvas.InterpolationMode := InterpolationMode;

    AImageHandle := PrepareImageHandle;
    if Stretch = smNoResize then
    begin
      APrevPixelOffsetMode := ACanvas.PixelOffsetMode;
      try
        ACanvas.PixelOffsetMode := PixelOffsetModeHalf;
        dxGpDrawImage(ACanvas.Handle, cxRectCenter(ARect, GetScaledSize), StateBounds[AImageIndex, AState], AImageHandle.Handle);
      finally
        ACanvas.PixelOffsetMode := APrevPixelOffsetMode;
      end;
    end
    else
      DrawStretched(AImageHandle, StateBounds[AImageIndex, AState].TopLeft, Margins.Margin);
  finally
    ACanvas.InterpolationMode := APrevInterpolationMode;
  end;
end;

procedure TdxSkinImage.DrawRTL(DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor;
  AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);
var
  R: TRect;
begin
  dxGPPaintCanvas.BeginPaint(DC, ARect);
  try
    R := ARect;
    dxGpRightToLeftDependentDraw(dxGPPaintCanvas, R, True,
      procedure
      begin
        DrawEx(dxGPPaintCanvas, R, AScaleFactor, AImageIndex, AState);
      end);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxSkinImage.InitializeInfo;

  function CalculateStateCount: Integer;
  var
    AState: TdxSkinElementState;
  begin
    Result := 0;
    for AState := Low(TdxSkinElementState) to High(TdxSkinElementState) do
    begin
      if AState in States then
        Inc(Result);
    end;
  end;

  function CalculateFrameSize: TSize;
  var
    AFrameCount: Integer;
  begin
    Result := cxSize(Texture.Width, Texture.Height);

    if IsVectorTexture then
      AFrameCount := ImageCount
    else
      AFrameCount := ImageCount * StateCount;

    if AFrameCount > 0 then
    begin
      if ImageLayout = ilHorizontal then
        Result.cx := Result.cx div AFrameCount
      else
        Result.cy := Result.cy div AFrameCount;
    end;
  end;

begin
  FPartsBoundsSourceSize := cxNullSize;
  FStateCount := CalculateStateCount;
  FSize := CalculateFrameSize;
  CheckCalculatePartsBounds(FSize);
end;

function TdxSkinImage.CreateTexture: TdxSmartImage;
begin
  Result := TdxSmartImage.Create;
end;

function TdxSkinImage.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TdxSkinImage.GetEmpty: Boolean;
begin
  Result := (FSourceName = '') and Texture.Empty;
end;

function TdxSkinImage.GetFileExt: string;
begin
  if IsVectorTexture then
    Result := dxSkinVectorImageExt
  else
    Result := dxSkinImageExt;
end;

function TdxSkinImage.GetGradientBeginColor: TColor;
begin
  Result := FGradientBeginColor.Value;
end;

function TdxSkinImage.GetGradientBeginColorReference: string;
begin
  Result := FGradientBeginColor.ValueReference;
end;

function TdxSkinImage.GetGradientEndColor: TColor;
begin
  Result := FGradientEndColor.Value;
end;

function TdxSkinImage.GetGradientEndColorReference: string;
begin
  Result := FGradientEndColor.ValueReference;
end;

function TdxSkinImage.GetIsGradientParamsAssigned: Boolean;
begin
  Result := cxColorIsValid(GradientBeginColor);
end;

function TdxSkinImage.GetIsVectorTexture: Boolean;
begin
  if FTextureIsVector = bDefault then
  begin
    if Supports(Texture, IdxVectorImage) then
      FTextureIsVector := bTrue
    else
      FTextureIsVector := bFalse;
  end;
  Result := FTextureIsVector = bTrue;
end;

function TdxSkinImage.GetName: string;
begin
  if Empty or (Owner = nil) then
    Result := ''
  else
    Result := Owner.Name + IfThen(Self = Owner.Glyph, '_Glyph', '_Image') + GetFileExt;
end;

function TdxSkinImage.GetPartBounds(APart: TdxSkinImagePart): TRect;
begin
  Result := FPartsBounds[APart];
end;

function TdxSkinImage.GetPartVisible(APart: TdxSkinImagePart): Boolean;
begin
  Result := FPartsVisibility[APart];
end;

function TdxSkinImage.GetSize: TSize;
begin
  CheckInfo;
  Result := FSize;
end;

function TdxSkinImage.GetSourceName: string;
begin
  Result := FSourceName;
  if (Result = '') and not Empty and (Owner <> nil) then
    Result := Owner.Path + Name;
end;

function TdxSkinImage.GetStateBounds(AImageIndex: Integer; AState: TdxSkinElementState): TRect;
var
  AFramesPerState: Integer;
begin
  CheckInfo;
  Result := FStateBounds[AState];
  if AImageIndex > 0 then
  begin
    if IsVectorTexture then
      AFramesPerState := 1
    else
      AFramesPerState := StateCount;

    if ImageLayout = ilHorizontal then
      OffsetRect(Result, AFramesPerState * AImageIndex * FPartsBoundsSourceSize.cx, 0)
    else
      OffsetRect(Result, 0, AFramesPerState * AImageIndex * FPartsBoundsSourceSize.cy)
  end;
end;

function TdxSkinImage.GetStateCount: Integer;
begin
  CheckInfo;
  Result := FStateCount;
end;

procedure TdxSkinImage.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
begin
  FImageCount := Owner.FReadImageCount;
  FMargins.LoadFromStream(AStream);
  AStream.ReadBuffer(FImageLayout, SizeOf(TdxSkinImageLayout));
  if AVersion >= 1.10 then
    AStream.ReadBuffer(FImageCount, SizeOf(FImageCount));
  AStream.ReadBuffer(FStates, SizeOf(TdxSkinElementStates));
  AStream.ReadBuffer(FStretch, SizeOf(FStretch));
  if AVersion >= 1.02 then
  begin
    FGradientBeginColor.DataRead(AStream, AVersion);
    FGradientEndColor.DataRead(AStream, AVersion);
    AStream.ReadBuffer(FGradient, SizeOf(FGradient));
  end;
  dxSkinReadSmartImageFromStream(AStream, Texture);
  if AVersion >= 1.15 then
    ColorPalettes.DataRead(AStream, AVersion);
  if AVersion >= 1.24 then
    AStream.ReadBuffer(FInterpolationMode, SizeOf(TdxGPInterpolationMode));
  IsDirty := True;
end;

procedure TdxSkinImage.DataWrite(AStream: TStream);
begin
  FMargins.SaveToStream(AStream);
  AStream.WriteBuffer(FImageLayout, SizeOf(TdxSkinImageLayout));
  AStream.WriteBuffer(FImageCount, SizeOf(FImageCount));
  AStream.WriteBuffer(FStates, SizeOf(TdxSkinElementStates));
  AStream.WriteBuffer(FStretch, SizeOf(FStretch));
  FGradientBeginColor.DataWrite(AStream);
  FGradientEndColor.DataWrite(AStream);
  AStream.WriteBuffer(FGradient, SizeOf(FGradient));
  dxSkinWriteSmartImageToStream(AStream, Texture);
  ColorPalettes.DataWrite(AStream);
  AStream.WriteBuffer(FInterpolationMode, SizeOf(TdxGPInterpolationMode));
end;

procedure TdxSkinImage.FlushCache;
begin
  FGradientBeginColor.FlushCache;
  FGradientEndColor.FlushCache;
  FColorPalettes.FlushCache;
end;

function TdxSkinImage.GetSourceDPI: Integer;
begin
  Result := dxDefaultDPI;
end;

procedure TdxSkinImage.SetColorPalettes(const Value: TdxSkinColorPalettes);
begin
  FColorPalettes.Assign(Value);
end;

procedure TdxSkinImage.SetGradientBeginColor(AValue: TColor);
begin
  FGradientBeginColor.Value := AValue;
end;

procedure TdxSkinImage.SetGradientBeginColorReference(const Value: string);
begin
  FGradientBeginColor.ValueReference := Value;
end;

procedure TdxSkinImage.SetGradientEndColor(AValue: TColor);
begin
  FGradientEndColor.Value := AValue;
end;

procedure TdxSkinImage.SetGradientEndColorReference(const Value: string);
begin
  FGradientEndColor.ValueReference := Value;
end;

procedure TdxSkinImage.SetGradientMode(AValue: TdxSkinGradientMode);
begin
  if AValue <> FGradient then
  begin
    FGradient := AValue;
    Changed;
  end;
end;

procedure TdxSkinImage.SetImageCount(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if ImageCount <> AValue then
  begin
    FImageCount := AValue;
    Changed;
  end;
end;

procedure TdxSkinImage.SetImageLayout(AValue: TdxSkinImageLayout);
begin
  if ImageLayout <> AValue then
  begin
    FImageLayout := AValue;
    Changed;
  end;
end;

procedure TdxSkinImage.SetInterpolationMode(AValue: TdxGPInterpolationMode);
begin
  if FInterpolationMode <> AValue then
  begin
    FInterpolationMode := AValue;
    Changed;
  end;
end;

procedure TdxSkinImage.SetMargins(AValue: TcxMargin);
begin
  FMargins.Assign(AValue);
end;

procedure TdxSkinImage.SetStates(AValue: TdxSkinElementStates);
begin
  if FStates <> AValue then
  begin
    FStates := AValue;
    Changed;
  end;
end;

procedure TdxSkinImage.SetStretch(AValue: TdxSkinStretchMode);
begin
  if Stretch <> AValue then
  begin
    FStretch := AValue;
    Changed;
  end;
end;

procedure TdxSkinImage.SetName(const AValue: string);
begin
  LoadFromFile(AValue);
end;

procedure TdxSkinImage.HandlerColorPaletteChanged(Sender: TObject; Changes: TdxSkinChanges);
begin
  Changed;
end;

procedure TdxSkinImage.HandlerMarginsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxSkinImage.HandlerTextureChanged(Sender: TObject);
begin
  FTextureIsVector := bDefault;
  Changed;
end;

{ TdxSkinGlyph }

procedure TdxSkinGlyph.AfterConstruction;
begin
  inherited;
  ScaleMargins := True;
end;

function TdxSkinGlyph.CreateTexture: TdxSmartImage;
begin
  Result := TdxSmartGlyph.Create;
  TdxSmartGlyphAccess(Result).FTransparent := False;
end;

function TdxSkinGlyph.GetSourceDPI: Integer;
begin
  Result := Texture.SourceDPI;
end;

function TdxSkinGlyph.GetTexture: TdxSmartGlyph;
begin
  Result := TdxSmartGlyph(inherited Texture);
end;

{ TdxSkinCustomObject }

constructor TdxSkinCustomObject.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create;
  FName := AName;
  FOwner := AOwner;
  FState := [sosUnused];
end;

procedure TdxSkinCustomObject.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    AssignCore(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinCustomObject.AssignCore(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TdxSkinCustomObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxSkinCustomObject.CancelUpdate;
begin
  Dec(FUpdateCount);
  FChanges := [];
end;

procedure TdxSkinCustomObject.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed(FChanges);
end;

procedure TdxSkinCustomObject.Changed(AChanges: TdxSkinChanges);
begin
  FChanges := FChanges + AChanges;
  if UpdateCount = 0 then
  begin
    DoChanged(AChanges);
    FChanges := [];
  end;
end;

procedure TdxSkinCustomObject.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
end;

procedure TdxSkinCustomObject.DataWrite(Stream: TStream);
begin
end;

procedure TdxSkinCustomObject.DoChanged(AChanges: TdxSkinChanges);
begin
  dxSkinChangeNotify(Self, OnChange, AChanges);
end;

procedure TdxSkinCustomObject.FlushCache;
begin
  // do nothing
end;

function TdxSkinCustomObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TdxSkinCustomObject.SetName(const AValue: string);
begin
  if AValue <> FName then
  begin
    FName := AValue;
    Changed([scStruct]);
  end;
end;

{ TdxSkinProperty }

class procedure TdxSkinProperty.Register;
begin
  if FRegisteredPropertyTypes = nil then
    FRegisteredPropertyTypes := TList.Create;
  FRegisteredPropertyTypes.Add(Self);
  RegisterClass(Self);
end;

class procedure TdxSkinProperty.Unregister;
begin
  UnregisterClass(Self);
  if FRegisteredPropertyTypes <> nil then
  begin
    FRegisteredPropertyTypes.Remove(Self);
    if FRegisteredPropertyTypes.Count = 0 then
      FreeAndNil(FRegisteredPropertyTypes);
  end;
end;

class function TdxSkinProperty.Description: string;
begin
  Result := StringReplace(ClassName, 'TdxSkin', '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'Property', '', [rfReplaceAll, rfIgnoreCase]);
end;

function TdxSkinProperty.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := SameText(Name, AProperty.Name);
end;

{ TdxSkinIntegerProperty }

procedure TdxSkinIntegerProperty.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinIntegerProperty then
    Value := TdxSkinIntegerProperty(Source).Value
  else
    inherited;
end;

function TdxSkinIntegerProperty.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinIntegerProperty) and
    (TdxSkinIntegerProperty(AProperty).Value = Value);
end;

procedure TdxSkinIntegerProperty.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
  Stream.ReadBuffer(FValue, SizeOf(FValue));
end;

procedure TdxSkinIntegerProperty.DataWrite(Stream: TStream);
begin
  Stream.WriteBuffer(FValue, SizeOf(FValue));
end;

procedure TdxSkinIntegerProperty.SetValue(AValue: Integer);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed([scContent]);
  end;
end;

{ TdxSkinBooleanProperty }

procedure TdxSkinBooleanProperty.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinBooleanProperty then
    Value := TdxSkinBooleanProperty(Source).Value
  else
    inherited;
end;

function TdxSkinBooleanProperty.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinBooleanProperty) and
    (TdxSkinBooleanProperty(AProperty).Value = Value)
end;

procedure TdxSkinBooleanProperty.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
  Stream.ReadBuffer(FValue, SizeOf(FValue));
end;

procedure TdxSkinBooleanProperty.DataWrite(Stream: TStream);
begin
  Stream.WriteBuffer(FValue, SizeOf(FValue));
end;

procedure TdxSkinBooleanProperty.SetValue(AValue: Boolean);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed([scContent]);
  end;
end;

{ TdxSkinColor }

constructor TdxSkinColor.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create(AOwner, AName);
  FValue := TdxSkinColorValue.Create(Self);
end;

procedure TdxSkinColor.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinColor then
    FValue.Assign(TdxSkinColor(Source).FValue)
  else
    inherited;
end;

function TdxSkinColor.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinColor) and TdxSkinColor(AProperty).FValue.Equals(FValue);
end;

procedure TdxSkinColor.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
  FValue.DataRead(Stream, AVersion);
end;

procedure TdxSkinColor.DataWrite(Stream: TStream);
begin
  FValue.DataWrite(Stream);
end;

procedure TdxSkinColor.FlushCache;
begin
  inherited FlushCache;
  FValue.FlushCache;
end;

function TdxSkinColor.GetValueAsAlphaColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(Value);
end;

function TdxSkinColor.GetValue: TColor;
begin
  Result := FValue.Value;
end;

function TdxSkinColor.GetValueReference: string;
begin
  Result := FValue.ValueReference;
end;

procedure TdxSkinColor.SetValue(const AValue: TColor);
begin
  FValue.Value := AValue;
end;

procedure TdxSkinColor.SetValueReference(const Value: string);
begin
  FValue.ValueReference := Value;
end;

{ TdxSkinAlphaColor }

constructor TdxSkinAlphaColor.Create(AOwner: TPersistent; const AName: string);
begin
  inherited;
  FValueAlpha := MaxByte;
end;

procedure TdxSkinAlphaColor.AssignCore(Source: TPersistent);
begin
  inherited;
  if Source is TdxSkinAlphaColor then
    ValueAlpha := TdxSkinAlphaColor(Source).ValueAlpha;
end;

procedure TdxSkinAlphaColor.DataRead(Stream: TStream; const AVersion: Double);
begin
  inherited;
  ValueAlpha := dxSkinReadIntegerFromStream(Stream);
end;

procedure TdxSkinAlphaColor.DataWrite(Stream: TStream);
begin
  inherited;
  dxSkinWriteIntegerToStream(Stream, ValueAlpha);
end;

function TdxSkinAlphaColor.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited and (AProperty is TdxSkinAlphaColor) and (TdxSkinAlphaColor(AProperty).ValueAlpha = ValueAlpha);
end;

function TdxSkinAlphaColor.GetValueAsAlphaColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(Value, ValueAlpha);
end;

procedure TdxSkinAlphaColor.SetValueAlpha(const Value: Byte);
begin
  if FValueAlpha <> Value then
  begin
    FValueAlpha := Value;
    Changed([scContent]);
  end;
end;

{ TdxSkinRectProperty }

constructor TdxSkinRectProperty.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create(AOwner, AName);
  FValue := TcxRect.Create(Self);
  FValue.OnChange := InternalHandler;
end;

destructor TdxSkinRectProperty.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

procedure TdxSkinRectProperty.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinRectProperty then
    Value := TdxSkinRectProperty(Source).Value
  else
    inherited;
end;

function TdxSkinRectProperty.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinRectProperty) and
    TdxSkinRectProperty(AProperty).Value.IsEqual(Value);
end;

procedure TdxSkinRectProperty.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
var
  ARect: TRect;
begin
  Stream.ReadBuffer(ARect, SizeOf(TRect));
  FValue.Rect := ARect;
end;

procedure TdxSkinRectProperty.DataWrite(Stream: TStream);
begin
  Stream.WriteBuffer(FValue.Rect, SizeOf(TRect));
end;

function TdxSkinRectProperty.GetValueByIndex(Index: Integer): Integer;
begin
  Result := FValue.GetValue(Index);
end;

procedure TdxSkinRectProperty.SetValue(Value: TcxRect);
begin
  FValue.Assign(Value);
end;

procedure TdxSkinRectProperty.SetValueByIndex(Index, Value: Integer);
begin
  FValue.SetValue(Index, Value);
end;

procedure TdxSkinRectProperty.InternalHandler(Sender: TObject);
begin
  Changed([scContent]);
end;

{ TdxSkinSizeProperty }

procedure TdxSkinSizeProperty.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinSizeProperty then
    Value := TdxSkinSizeProperty(Source).Value
  else
    inherited;
end;

function TdxSkinSizeProperty.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinSizeProperty) and
    cxSizeIsEqual(Value, TdxSkinSizeProperty(AProperty).Value);
end;

function TdxSkinSizeProperty.GetValueByIndex(Index: Integer): Integer;
begin
  if Index = 0 then
    Result := FValue.cx
  else
    Result := FValue.cy
end;

procedure TdxSkinSizeProperty.SetValueByIndex(Index, Value: Integer);
var
  AValue: TSize;
begin
  AValue := FValue;
  if Index = 0 then
    AValue.cx := Value
  else
    AValue.cy := Value;
  SetValue(AValue);
end;

procedure TdxSkinSizeProperty.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
  Stream.ReadBuffer(FValue, SizeOf(FValue));
end;

procedure TdxSkinSizeProperty.DataWrite(Stream: TStream);
begin
  Stream.WriteBuffer(FValue, SizeOf(FValue));
end;

procedure TdxSkinSizeProperty.SetValue(const AValue: TSize);
begin
  if not cxSizeIsEqual(AValue, Value) then
  begin
    FValue := Value;
    Changed([scContent]);
  end;
end;

{ TdxSkinBorder }

constructor TdxSkinBorder.Create(AOwner: TPersistent; AKind: TcxBorder);
const
  BorderNames: array[TcxBorder] of string = (sdxLeft, sdxTop, sdxRight, sdxBottom);
begin
  inherited Create(AOwner, BorderNames[AKind]);
  FColor := TdxSkinColorValue.Create(Self, clNone);
  FKind := AKind;
  FThin := 1;
end;

procedure TdxSkinBorder.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinBorder then
  begin
    FColor.Assign(TdxSkinBorder(Source).FColor);
    FKind := TdxSkinBorder(Source).Kind;
    Thin := TdxSkinBorder(Source).Thin;
  end;
end;

function TdxSkinBorder.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := (AProperty is TdxSkinBorder) and
    (TdxSkinBorder(AProperty).Kind = Kind) and
    (TdxSkinBorder(AProperty).Thin = Thin) and
    (TdxSkinBorder(AProperty).FColor.Equals(FColor));
end;

procedure TdxSkinBorder.Draw(DC: HDC; const ABounds: TRect);
begin
  if Color <> clNone then
  begin
    dxGPPaintCanvas.BeginPaint(DC, ABounds);
    try
      DrawEx(dxGPPaintCanvas, ABounds);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxSkinBorder.DrawEx(ACanvas: TdxGPCanvas; const ABounds: TRect);
begin
  if Color = clNone then
    Exit;

  case Kind of
    bLeft:
      ACanvas.FillRectangle(Rect(ABounds.Left, ABounds.Top, ABounds.Left + Thin, ABounds.Bottom), dxColorToAlphaColor(Color));
    bTop:
      ACanvas.FillRectangle(Rect(ABounds.Left, ABounds.Top, ABounds.Right, ABounds.Top + Thin), dxColorToAlphaColor(Color));
    bRight:
      ACanvas.FillRectangle(Rect(ABounds.Right - Thin, ABounds.Top, ABounds.Right, ABounds.Bottom), dxColorToAlphaColor(Color));
    bBottom:
      ACanvas.FillRectangle(Rect(ABounds.Left, ABounds.Bottom - Thin, ABounds.Right, ABounds.Bottom), dxColorToAlphaColor(Color));
  end;
end;

procedure TdxSkinBorder.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
  FColor.DataRead(Stream, AVersion);
  Stream.ReadBuffer(FThin, SizeOf(FThin));
end;

procedure TdxSkinBorder.DataWrite(Stream: TStream);
begin
  FColor.DataWrite(Stream);
  Stream.WriteBuffer(FThin, SizeOf(FThin));
end;

procedure TdxSkinBorder.FlushCache;
begin
  inherited FlushCache;
  FColor.FlushCache;
end;

procedure TdxSkinBorder.ResetToDefaults;
begin
  FColor.ResetToDefaults;
  Thin := 1;
end;

function TdxSkinBorder.GetColor: TColor;
begin
  Result := FColor.Value;
end;

function TdxSkinBorder.GetColorReference: string;
begin
  Result := FColor.ValueReference;
end;

function TdxSkinBorder.GetContentMargin: Integer;
begin
  if Color <> clNone then
    Result := Thin
  else
    Result := 0;
end;

procedure TdxSkinBorder.SetColor(AValue: TColor);
begin
  FColor.Value := AValue;
end;

procedure TdxSkinBorder.SetColorReference(const Value: string);
begin
  FColor.ValueReference := Value;
end;

procedure TdxSkinBorder.SetThin(AValue: Integer);
begin
  if AValue <> FThin then
  begin
    FThin := AValue;
    Changed([scContent]);
  end;
end;

{ TdxSkinBorders }

constructor TdxSkinBorders.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create(AOwner, AName);
  CreateBorders;
end;

destructor TdxSkinBorders.Destroy;
begin
  DeleteBorders;
  inherited Destroy;
end;

procedure TdxSkinBorders.AssignCore(ASource: TPersistent);
var
  ABorder: TcxBorder;
begin
  if ASource is TdxSkinBorders then
  begin
    for ABorder := Low(TcxBorder) to High(TcxBorder) do
      FBorders[ABorder].Assign(TdxSkinBorders(ASource).FBorders[ABorder])
  end
  else
    inherited;
end;

function TdxSkinBorders.Compare(AProperty: TdxSkinProperty): Boolean;
var
  ASide: TcxBorder;
begin
  Result := (AProperty is TdxSkinBorders);
  if Result then
  begin
    for ASide := Low(TcxBorder) to High(TcxBorder) do
      Result := Result and Items[ASide].Compare(TdxSkinBorders(AProperty).Items[ASide]);
  end;
end;

procedure TdxSkinBorders.Draw(ACanvas: TdxGPCanvas; const ABounds: TRect);
var
  ASide: TcxBorder;
begin
  for ASide := Low(TcxBorder) to High(TcxBorder) do
    Items[ASide].DrawEx(ACanvas, ABounds);
end;

procedure TdxSkinBorders.CreateBorders;
var
  ASide: TcxBorder;
begin
  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    FBorders[ASide] := TdxSkinBorder.Create(Self, ASide);
    FBorders[ASide].OnChange := SubItemChanged;
  end;
end;

procedure TdxSkinBorders.DeleteBorders;
var
  ASide: TcxBorder;
begin
  for ASide := Low(TcxBorder) to High(TcxBorder) do
    FreeAndNil(FBorders[ASide])
end;

procedure TdxSkinBorders.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
var
  ASide: TcxBorder;
begin
  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    if AVersion <= 1.09 then
    begin
      dxSkinReadStringFromStream(AStream); // Skip ClassName
      dxSkinReadStringFromStream(AStream); // Skip Name
    end;
    Items[ASide].DataRead(AStream, AVersion);
  end;
end;

procedure TdxSkinBorders.DataWrite(AStream: TStream);
var
  ASide: TcxBorder;
begin
  for ASide := Low(TcxBorder) to High(TcxBorder) do
    Items[ASide].DataWrite(AStream);
end;

procedure TdxSkinBorders.FlushCache;
var
  ASide: TcxBorder;
begin
  inherited FlushCache;
  for ASide := Low(TcxBorder) to High(TcxBorder) do
    Items[ASide].FlushCache;
end;

procedure TdxSkinBorders.ResetToDefaults;
var
  ASide: TcxBorder;
begin
  for ASide := Low(TcxBorder) to High(TcxBorder) do
    Items[ASide].ResetToDefaults;
end;

procedure TdxSkinBorders.SubItemChanged(ASender: TObject; AChanges: TdxSkinChanges);
begin
  Changed(AChanges);
end;

function TdxSkinBorders.GetBorder(ABorder: TcxBorder): TdxSkinBorder;
begin
  Result := FBorders[ABorder];
end;

function TdxSkinBorders.GetBorderByIndex(Index: Integer): TdxSkinBorder;
begin
  Result := FBorders[TcxBorder(Index)];
end;

function TdxSkinBorders.GetContentMargins: TRect;
begin
  Result := Rect(Left.ContentMargin, Top.ContentMargin, Right.ContentMargin, Bottom.ContentMargin);
end;

procedure TdxSkinBorders.SetBorderByIndex(Index: Integer; AValue: TdxSkinBorder);
begin
  FBorders[TcxBorder(Index)].Assign(AValue);
end;

{ TdxSkinWideStringProperty }

procedure TdxSkinWideStringProperty.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinWideStringProperty then
    Value := TdxSkinWideStringProperty(Source).Value
  else
    inherited;
end;

procedure TdxSkinWideStringProperty.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
  Value := dxSkinReadWideStringFromStream(Stream);
end;

procedure TdxSkinWideStringProperty.DataWrite(Stream: TStream);
begin
  dxSkinWriteWideStringToStream(Stream, Value);
end;

{ TdxSkinStringProperty }

procedure TdxSkinStringProperty.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinStringProperty then
    Value := TdxSkinStringProperty(Source).Value
  else
    inherited;
end;

function TdxSkinStringProperty.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinStringProperty) and
    (AnsiCompareStr(TdxSkinStringProperty(AProperty).Value, Value) = 0);
end;

procedure TdxSkinStringProperty.DataRead(Stream: TStream; const AVersion: TdxSkinVersion);
begin
  Value := dxSkinReadStringFromStream(Stream);
end;

procedure TdxSkinStringProperty.DataWrite(Stream: TStream);
begin
  dxSkinWriteStringToStream(Stream, Value);
end;

procedure TdxSkinStringProperty.SetValue(const AValue: string);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed([scContent]);
  end;
end;

{ TdxSkinAlternateImageAttributes }

constructor TdxSkinAlternateImageAttributes.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create(AOwner, AName);
  FAlpha := MaxByte;
  FGradientBeginColor := TdxSkinColorValue.Create(Self, clNone);
  FGradientEndColor := TdxSkinColorValue.Create(Self, clNone);
  FBorders := TdxSkinBorders.Create(Self, sdxBorders);
  FBorders.OnChange := BordersChanged;
  FBordersInner := TdxSkinBorders.Create(Self, sdxBordersInner);
  FBordersInner.OnChange := BordersChanged;
  FContentOffsets := TcxRect.Create(Self);
  FContentOffsets.OnChange := ContentOffsetsChanged;
end;

destructor TdxSkinAlternateImageAttributes.Destroy;
begin
  FreeAndNil(FContentOffsets);
  FreeAndNil(FBordersInner);
  FreeAndNil(FBorders);
  inherited Destroy;
end;

procedure TdxSkinAlternateImageAttributes.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinAlternateImageAttributes then
  begin
    Alpha := TdxSkinAlternateImageAttributes(Source).Alpha;
    Borders := TdxSkinAlternateImageAttributes(Source).Borders;
    BordersInner := TdxSkinAlternateImageAttributes(Source).BordersInner;
    Gradient := TdxSkinAlternateImageAttributes(Source).Gradient;
    FGradientBeginColor.Assign(TdxSkinAlternateImageAttributes(Source).FGradientBeginColor);
    FGradientEndColor.Assign(TdxSkinAlternateImageAttributes(Source).FGradientEndColor);
    ContentOffsets.Rect := TdxSkinAlternateImageAttributes(Source).ContentOffsets.Rect;
  end
  else
    inherited;
end;

procedure TdxSkinAlternateImageAttributes.BordersChanged(ASender: TObject; AChanges: TdxSkinChanges);
begin
  Changed(AChanges);
end;

function TdxSkinAlternateImageAttributes.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinAlternateImageAttributes) and
    (TdxSkinAlternateImageAttributes(AProperty).Alpha = Alpha) and
    (TdxSkinAlternateImageAttributes(AProperty).Gradient = Gradient) and
    (TdxSkinAlternateImageAttributes(AProperty).FGradientBeginColor.Equals(FGradientBeginColor)) and
    (TdxSkinAlternateImageAttributes(AProperty).FGradientEndColor.Equals(FGradientEndColor)) and
    (TdxSkinAlternateImageAttributes(AProperty).Borders.Compare(Borders)) and
    (TdxSkinAlternateImageAttributes(AProperty).BordersInner.Compare(BordersInner)) and
    (TdxSkinAlternateImageAttributes(AProperty).ContentOffsets.IsEqual(ContentOffsets.Rect));
end;

procedure TdxSkinAlternateImageAttributes.ContentOffsetsChanged(ASender: TObject);
begin
  Changed([scContent]);
end;

procedure TdxSkinAlternateImageAttributes.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
begin
  AStream.ReadBuffer(FAlpha, SizeOf(FAlpha));
  AStream.ReadBuffer(FGradient, SizeOf(FGradient));
  FGradientBeginColor.DataRead(AStream, AVersion);
  FGradientEndColor.DataRead(AStream, AVersion);
  Borders.DataRead(AStream, AVersion);
  BordersInner.DataRead(AStream, AVersion);
  ContentOffsets.LoadFromStream(AStream);
end;

procedure TdxSkinAlternateImageAttributes.DataWrite(AStream: TStream);
begin
  AStream.WriteBuffer(FAlpha, SizeOf(FAlpha));
  AStream.WriteBuffer(FGradient, SizeOf(FGradient));
  FGradientBeginColor.DataWrite(AStream);
  FGradientEndColor.DataWrite(AStream);
  Borders.DataWrite(AStream);
  BordersInner.DataWrite(AStream);
  ContentOffsets.SaveToStream(AStream);
end;

procedure TdxSkinAlternateImageAttributes.FlushCache;
begin
  inherited FlushCache;
  Borders.FlushCache;
  BordersInner.FlushCache;
  FGradientBeginColor.FlushCache;
  FGradientEndColor.FlushCache;
end;

procedure TdxSkinAlternateImageAttributes.Draw(ACanvas: TdxGPCanvas; const R: TRect);
var
  ARect: TRect;
begin
  ARect := cxRectContent(R, ContentOffsets.Rect);
  TdxSkinRender.FillRectByGradient(ACanvas, ARect, GradientBeginColor, GradientEndColor, Gradient, Alpha);
  Borders.Draw(ACanvas, ARect);
  BordersInner.Draw(ACanvas, cxRectContent(ARect, Borders.ContentMargins));
end;

function TdxSkinAlternateImageAttributes.GetGradientBeginColor: TColor;
begin
  Result := FGradientBeginColor.Value;
end;

function TdxSkinAlternateImageAttributes.GetGradientBeginColorReference: string;
begin
  Result := FGradientBeginColor.ValueReference;
end;

function TdxSkinAlternateImageAttributes.GetGradientEndColor: TColor;
begin
  Result := FGradientEndColor.Value;
end;

function TdxSkinAlternateImageAttributes.GetGradientEndColorReference: string;
begin
  Result := FGradientEndColor.ValueReference;
end;

function TdxSkinAlternateImageAttributes.GetIsAlphaUsed: Boolean;
begin
  Result := (Alpha < MaxByte) or not cxRectIsEqual(ContentOffsets.Rect, cxNullRect);
end;

procedure TdxSkinAlternateImageAttributes.SetAlpha(AValue: Byte);
begin
  if Alpha <> AValue then
  begin
    FAlpha := AValue;
    Changed([scContent]);
  end;
end;

procedure TdxSkinAlternateImageAttributes.SetBorders(AValue: TdxSkinBorders);
begin
  FBorders.Assign(AValue);
end;

procedure TdxSkinAlternateImageAttributes.SetBordersInner(AValue: TdxSkinBorders);
begin
  FBordersInner.Assign(AValue);
end;

procedure TdxSkinAlternateImageAttributes.SetContentOffsets(AValue: TcxRect);
begin
  FContentOffsets.Assign(AValue);
end;

procedure TdxSkinAlternateImageAttributes.SetGradientBeginColor(AValue: TColor);
begin
  FGradientBeginColor.Value := AValue;
end;

procedure TdxSkinAlternateImageAttributes.SetGradientBeginColorReference(const Value: string);
begin
  FGradientBeginColor.ValueReference := Value;
end;

procedure TdxSkinAlternateImageAttributes.SetGradientEndColor(AValue: TColor);
begin
  FGradientEndColor.Value := AValue;
end;

procedure TdxSkinAlternateImageAttributes.SetGradientEndColorReference(const Value: string);
begin
  FGradientEndColor.ValueReference := Value;
end;

procedure TdxSkinAlternateImageAttributes.SetGradientMode(AValue: TdxSkinGradientMode);
begin
  if AValue <> FGradient then
  begin
    FGradient := AValue;
    Changed([scContent]);
  end;
end;

{ TdxSkinControlGroup }

constructor TdxSkinControlGroup.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create(AOwner, AName);
  FElements := TdxSkinElements.Create(Self);
  FElements.OnChange := SubItemChanged;
end;

destructor TdxSkinControlGroup.Destroy;
begin
  FreeAndNil(FElements);
  inherited Destroy;
end;

procedure TdxSkinControlGroup.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinControlGroup then
    FElements.Assign(TdxSkinControlGroup(Source).FElements);
  inherited;
end;

function TdxSkinControlGroup.AddElement(const AName: string): TdxSkinElement;
begin
  Result := AddElementEx(AName, TdxSkinElement);
end;

function TdxSkinControlGroup.AddElementEx(
  const AName: string; AElementClass: TdxSkinElementClass): TdxSkinElement;
begin
  Result := FElements.Add(AName, AElementClass);
end;

procedure TdxSkinControlGroup.Clear;
begin
  inherited Clear;
  FElements.Clear;
end;

procedure TdxSkinControlGroup.ClearModified;
var
  I: Integer;
begin
  FModified := False;
  for I := 0 to Count - 1 do
    Elements[I].Modified := False;
end;

procedure TdxSkinControlGroup.Delete(AIndex: Integer);
begin
  FElements.FreeAndDelete(AIndex);
end;

procedure TdxSkinControlGroup.Dormant;
begin
  FElements.Dormant;
end;

procedure TdxSkinControlGroup.ExchangeElements(AIndex1, AIndex2: Integer);
begin
  FElements.Exchange(AIndex1, AIndex2);
end;

procedure TdxSkinControlGroup.RemoveElement(AElement: TdxSkinElement);
begin
  FElements.FreeAndRemove(AElement);
end;

procedure TdxSkinControlGroup.RemoveElementByName(const AElementName: string);
var
  AElement: TdxSkinElement;
begin
  if GetElementByName(AElementName, AElement) then
    RemoveElement(AElement);
end;

function TdxSkinControlGroup.GetElementByName(const AName: string): TdxSkinElement;
begin
  if not GetElementByName(AName, Result) then
    Result := nil;
end;

function TdxSkinControlGroup.GetElementByName(const AName: string; out AElement: TdxSkinElement): Boolean;
begin
  Result := FElements.Find(AName, AElement);
end;

procedure TdxSkinControlGroup.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
begin
  FElements.DataRead(AStream, AVersion);
  FProperties.DataRead(AStream, AVersion);
end;

procedure TdxSkinControlGroup.DataWrite(AStream: TStream);
begin
  FElements.DataWrite(AStream);
  FProperties.DataWrite(AStream);
end;

procedure TdxSkinControlGroup.FlushCache;
begin
  inherited FlushCache;
  FElements.FlushCache;
end;

procedure TdxSkinControlGroup.Sort(ASortChildren: Boolean = False);
begin
  inherited Sort(ASortChildren);
  FElements.Sort(ASortChildren);
end;

function TdxSkinControlGroup.GetCount: Integer;
begin
  Result := FElements.Count;
end;

function TdxSkinControlGroup.GetElement(AIndex: Integer): TdxSkinElement;
begin
  Result := FElements[AIndex] as TdxSkinElement;
end;

function TdxSkinControlGroup.GetHasMissingElements: Boolean;
var
  I: Integer;
begin
  Result := sosUnassigned in State;
  if not Result then
  begin
    for I := 0 to Count - 1 do
    begin
      Result := sosUnassigned in Elements[I].State;
      if Result then Break;
    end;
  end;
end;

function TdxSkinControlGroup.GetSkin: TdxSkin;
begin
  Result := GetOwner as TdxSkin;
end;

procedure TdxSkinControlGroup.SetElement(AIndex: Integer; AElement: TdxSkinElement);
begin
  Elements[AIndex].Assign(AElement);
end;

{ TdxSkinElement }

constructor TdxSkinElement.Create(AOwner: TPersistent; const AName: string);
begin
  inherited Create(AOwner, AName);
  FAlpha := MaxByte;
  FCacheCapacity := TdxSkinElementCacheList.DefaultCapacity;
  FColor := TdxSkinColorValue.Create(Self, clDefault);
  FTextColor := TdxSkinColorValue.Create(Self, clDefault);
  FImage := TdxSkinImage.Create(Self);
  FImage.OnChange := SubItemChanged;
  FContentOffset := TcxRect.Create(Self);
  FContentOffset.OnChange := SubItemChanged;
  FGlyph := TdxSkinImage.Create(Self);
  FGlyph.OnChange := SubItemChanged;
  FBorders := TdxSkinBorders.Create(Self, sdxBorders);
  FBorders.OnChange := BordersChanged;
  FMinSize := TcxSize.Create(Self);
  FMinSize.OnChange := SubItemChanged;
end;

destructor TdxSkinElement.Destroy;
begin
  UseCache := False;
  FreeAndNil(FMinSize);
  FreeAndNil(FContentOffset);
  FreeAndNil(FImage);
  FreeAndNil(FGlyph);
  FreeAndNil(FBorders);
  inherited Destroy;
end;

function TdxSkinElement.AddAlternateImageAttributes(
  AState: TdxSkinElementState; AImageIndex: Integer): TdxSkinAlternateImageAttributes;
begin
  Result := TdxSkinAlternateImageAttributes(AddProperty(
    dxSkinGetAlternateImageAttrsPropertyName(AState, AImageIndex),
    TdxSkinAlternateImageAttributes));
end;

procedure TdxSkinElement.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinElement then
  begin
    Image.Assign(TdxSkinElement(Source).Image);
    Glyph.Assign(TdxSkinElement(Source).Glyph);
    Alpha := TdxSkinElement(Source).Alpha;
    ContentOffset := TdxSkinElement(Source).ContentOffset;
    Borders := TdxSkinElement(Source).Borders;
    MinSize := TdxSkinElement(Source).MinSize;
    FColor.Assign(TdxSkinElement(Source).FColor);
    FTextColor.Assign(TdxSkinElement(Source).FTextColor);
    UseCache := TdxSkinElement(Source).UseCache;
  end;
  inherited;
end;

function TdxSkinElement.CanUseAlternateImageSet(AImageIndex: Integer; AState: TdxSkinElementState;
  ALowColorsMode: Boolean; out AStateAttributes: TdxSkinAlternateImageAttributes): Boolean;
begin
  Result := Image.Empty or (dxSkinsUseImageSet = imsAlternate) or ALowColorsMode and (dxSkinsUseImageSet = imsDefault);
  if Result then
  begin
    Image.CheckState(AState, AImageIndex);
    CheckAlternateImageSet(AImageIndex);
    AStateAttributes := FAlternateImageSet[AState];
    Result := AStateAttributes <> nil;
  end;
end;

procedure TdxSkinElement.DoChanged(AChanges: TdxSkinChanges);
begin
  if FCache <> nil then
    FCache.Flush;
  if scStruct in AChanges then
    AlternateImageSetDirty := True;
  inherited;
end;

procedure TdxSkinElement.CheckAlternateImageSet(AIndex: Integer);
var
  AProperty: TdxSkinProperty;
  AState: TdxSkinElementState;
begin
  AlternateImageSetDirty := AlternateImageSetDirty or (AlternateImageSetIndex <> AIndex);
  if AlternateImageSetDirty then
  begin
    for AState := Low(TdxSkinElementState) to High(TdxSkinElementState) do
    begin
      if GetPropertyByName(dxSkinGetAlternateImageAttrsPropertyName(AState, AIndex), AProperty) and
        (AProperty is TdxSkinAlternateImageAttributes)
      then
        FAlternateImageSet[AState] := TdxSkinAlternateImageAttributes(AProperty)
      else
        FAlternateImageSet[AState] := nil;
    end;
    FAlternateImageSetIndex := AIndex;
    FAlternateImageSetDirty := False;
  end;
end;

function TdxSkinElement.Compare(AElement: TdxSkinElement): Boolean;
begin
  Result := SameText(AElement.Name, Name) and (Color = AElement.Color) and
    (Alpha = AElement.Alpha) and (TextColor = AElement.TextColor) and
    MinSize.IsEqual(AElement.MinSize) and ContentOffset.IsEqual(AElement.ContentOffset.Rect) and
    Borders.Compare(AElement.Borders) and FProperties.Compare(AElement.FProperties) and
    Image.Compare(AElement.Image) and Glyph.Compare(AElement.Glyph);
end;

function TdxSkinElement.CalculateMinSize: TSize;
begin
  Result := MinSize.Size;
  if Image.Stretch = smNoResize then
    Result := cxSizeMax(Result, Image.Size);
  if Glyph.Stretch = smNoResize then
    Result := cxSizeMax(Result, Glyph.Size);
end;

function TdxSkinElement.GetGlyphColorPalette(AState: TdxSkinElementState): IdxColorPalette;
begin
  Result := Glyph.GetPalette(AState, False);
  if Result = nil then
    Result := Group.Skin.GetGlyphColorPalette(AState);
end;

function TdxSkinElement.GetTextColor(AState: TcxButtonState; const APropertyPrefix: string = ''): TColor;
var
  AProperty: TdxSkinProperty;
begin
  Result := clDefault;
  if dxSkinElementTextColorPropertyNames[AState] <> '' then
  begin
    if GetPropertyByName(APropertyPrefix + dxSkinElementTextColorPropertyNames[AState], AProperty) then
      Result := (AProperty as TdxSkinColor).Value;
  end;

  if Result = clDefault then
  begin
    if AState <> cxbsNormal then
      Result := GetTextColor(cxbsNormal, APropertyPrefix)
    else
      Result := TextColor;
  end;
end;

procedure TdxSkinElement.Dormant;
begin
  if UseCache then
    FCache.Flush;
  Image.Dormant;
  Glyph.Dormant;
end;

procedure TdxSkinElement.Draw(DC: HDC; const ARect: TRect; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);
begin
  Draw(DC, ARect, dxSystemScaleFactor, AImageIndex, AState);
end;

procedure TdxSkinElement.Draw(DC: HDC; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; AImageIndex: Integer; AState: TdxSkinElementState);
begin
  if not cxRectIsEmpty(ARect) and RectVisible(DC, ARect) then
  begin
    if UseCache then
      FCache.DrawElement(DC, Self, ARect, AScaleFactor, AState, AImageIndex)
    else
    begin
      dxGPPaintCanvas.BeginPaint(DC, ARect);
      try
        DrawEx(dxGPPaintCanvas, ARect, AScaleFactor, AImageIndex, AState);
      finally
        dxGPPaintCanvas.EndPaint;
      end;
    end;
  end;
end;

procedure TdxSkinElement.DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect;
  AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);
begin
  DrawEx(ACanvas, ARect, dxSystemScaleFactor, AImageIndex, AState);
end;

procedure TdxSkinElement.DrawEx(ACanvas: TdxGPCanvas; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; AImageIndex: Integer; AState: TdxSkinElementState);
var
  AAttributes: TdxSkinAlternateImageAttributes;
begin
  ACanvas.FillRectangle(ARect, dxColorToAlphaColor(Color, Alpha));

  if CanUseAlternateImageSet(AImageIndex, AState, ACanvas.IsLowColorsMode, AAttributes) then
    AAttributes.Draw(ACanvas, ARect)
  else
    Image.DrawEx(ACanvas, ARect, AScaleFactor, AImageIndex, AState);

  Borders.Draw(ACanvas, ARect);
  Glyph.DrawEx(ACanvas, cxRectContent(ARect, ContentOffset.Rect), AScaleFactor, AImageIndex, AState);
end;

procedure TdxSkinElement.DrawRTL(DC: HDC; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);
var
  R: TRect;
begin
  if not cxRectIsEmpty(ARect) and RectVisible(DC, ARect) then
  begin
    dxGPPaintCanvas.BeginPaint(DC, ARect);
    try
      R := ARect;
      dxGpRightToLeftDependentDraw(dxGPPaintCanvas, R, True,
        procedure
        begin
          if UseCache then
            FCache.DrawElement(dxGPPaintCanvas, Self, R, AScaleFactor, AState, AImageIndex)
          else
            DrawEx(dxGPPaintCanvas, R, AScaleFactor, AImageIndex, AState);
        end);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

function TdxSkinElement.GetImage(AImageIndex: Integer; ASize: TSize;
  AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal): TdxSmartImage;
var
  ACanvas: TdxGPCanvas;
begin
  if (ASize.cx = 0) or (ASize.cy = 0) then
    ASize := AScaleFactor.Apply(CalculateMinSize);

  Result := TdxSmartImage.CreateSize(ASize);
  ACanvas := Result.CreateCanvas;
  try
    DrawEx(ACanvas, Result.ClientRect, AScaleFactor, AImageIndex, AState);
  finally
    ACanvas.Free;
  end;
end;

function TdxSkinElement.GetImage(AImageIndex: Integer; const ASize: TSize; AState: TdxSkinElementState = esNormal): TdxSmartImage;
begin
  Result := GetImage(AImageIndex, ASize, dxSystemScaleFactor, AState);
end;

procedure TdxSkinElement.SetStateMapping(AStateOrder: array of TdxSkinElementState);
begin
  FImage.SetStateMapping(AStateOrder);
  FGlyph.SetStateMapping(AStateOrder);
end;

procedure TdxSkinElement.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
begin
  FColor.DataRead(AStream, AVersion);
  AStream.ReadBuffer(FAlpha, SizeOf(FAlpha));
  if AVersion < 1.10 then
    AStream.ReadBuffer(FReadImageCount, SizeOf(FReadImageCount));
  ContentOffset.LoadFromStream(AStream);
  Glyph.DataRead(AStream, AVersion);
  Image.DataRead(AStream, AVersion);
  Borders.DataRead(AStream, AVersion);
  FTextColor.DataRead(AStream, AVersion);
  MinSize.LoadFromStream(AStream);
  FProperties.DataRead(AStream, AVersion);
end;

procedure TdxSkinElement.DataWrite(AStream: TStream);
begin
  FColor.DataWrite(AStream);
  AStream.WriteBuffer(FAlpha, SizeOf(Alpha));
  ContentOffset.SaveToStream(AStream);
  Glyph.DataWrite(AStream);
  Image.DataWrite(AStream);
  Borders.DataWrite(AStream);
  FTextColor.DataWrite(AStream);
  MinSize.SaveToStream(AStream);
  FProperties.DataWrite(AStream);
end;

procedure TdxSkinElement.FlushCache;
begin
  inherited FlushCache;
  FColor.FlushCache;
  FTextColor.FlushCache;
  FGlyph.FlushCache;
  FImage.FlushCache;
  Borders.FlushCache;
  if UseCache then
    FCache.Flush;
end;

procedure TdxSkinElement.BordersChanged(ASender: TObject; AChanges: TdxSkinChanges);
begin
  Changed(AChanges);
end;

procedure TdxSkinElement.SubItemChanged(ASender: TObject);
begin
  Changed([scContent]);
end;

function TdxSkinElement.GetColor: TColor;
begin
  Result := FColor.Value;
end;

function TdxSkinElement.GetColorReference: string;
begin
  Result := FColor.ValueReference;
end;

function TdxSkinElement.GetGroup: TdxSkinControlGroup;
begin
  Result := GetOwner as TdxSkinControlGroup;
end;

function TdxSkinElement.GetImageCount: Integer;
begin
  Result := Image.ImageCount;
end;

function TdxSkinElement.GetIsAlphaUsed: Boolean;
begin
  //todo: check for alternate states
  if Image.Empty then
    Result := not cxColorIsValid(Color) or (Alpha < 255)
  else
    Result := not cxColorIsValid(Color) and Image.Texture.IsAlphaUsed;
end;

function TdxSkinElement.GetPath: string;
begin
  Result := Group.Name + PathDelim;
end;

function TdxSkinElement.GetSize: TSize;
begin
  Result := Image.Size;
end;

function TdxSkinElement.GetStates: TdxSkinElementStates;
begin
  Result := Image.States;
end;

function TdxSkinElement.GetTextColorPropertyValue: TColor;
begin
  Result := FTextColor.Value;
end;

function TdxSkinElement.GetTextColorReference: string;
begin
  Result := FTextColor.ValueReference;
end;

function TdxSkinElement.GetUseCache: Boolean;
begin
  Result := FCache <> nil;
end;

procedure TdxSkinElement.SetAlpha(AValue: Byte);
begin
  if Alpha <> AValue then
  begin
    FAlpha := AValue;
    Changed([scContent]);
  end;
end;

procedure TdxSkinElement.SetBorders(AValue: TdxSkinBorders);
begin
  FBorders.Assign(AValue);
end;

procedure TdxSkinElement.SetCacheCapacity(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if CacheCapacity <> AValue then
  begin
    FCacheCapacity := AValue;
    if FCache <> nil then
      FCache.Capacity := CacheCapacity;
  end;
end;

procedure TdxSkinElement.SetColor(AValue: TColor);
begin
  FColor.Value := AValue;
end;

procedure TdxSkinElement.SetColorReference(const Value: string);
begin
  FColor.ValueReference := Value;
end;

procedure TdxSkinElement.SetContentOffset(AValue: TcxRect);
begin
  ContentOffset.Assign(AValue);
end;

procedure TdxSkinElement.SetGlyph(AValue: TdxSkinImage);
begin
  Glyph.Assign(AValue);
end;

procedure TdxSkinElement.SetImage(AValue: TdxSkinImage);
begin
  Image.Assign(AValue);
end;

procedure TdxSkinElement.SetMinSize(AValue: TcxSize);
begin
  FMinSize.Assign(AValue);
end;

procedure TdxSkinElement.SetTextColorPropertyValue(AValue: TColor);
begin
  FTextColor.Value := AValue;
end;

procedure TdxSkinElement.SetTextColorReference(const Value: string);
begin
  FTextColor.ValueReference := Value;
end;

procedure TdxSkinElement.SetUseCache(AValue: Boolean);
begin
  if AValue <> UseCache then
  begin
    if AValue then
      FCache := TdxSkinElementCacheList.Create(CacheCapacity)
    else
      FreeAndNil(FCache);
  end;
end;

{  TdxSkinEmptyElement }

procedure TdxSkinEmptyElement.Draw(DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor;
  AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);
var
  RedBrush: HBRUSH;
begin
  FillRect(DC, ARect, GetStockObject(WHITE_BRUSH));
  RedBrush := CreateSolidBrush(clRed);
  FrameRect(DC, ARect, RedBrush);
  DeleteObject(RedBrush);
end;

{  TdxSkinPartStream }

constructor TdxSkinPartStream.Create(ASource: TStream; const APosStart, ASize: LongInt);
begin
  inherited Create;
  FPosEnd := APosStart + ASize;
  FPosStart := APosStart;
  FSource := ASource;
end;

function TdxSkinPartStream.GetSize: Int64;
begin
  Result := FPosEnd - FPosStart;
end;

function TdxSkinPartStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Source.Read(Buffer, Count);
end;

function TdxSkinPartStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  ANewPos: Longint;
begin
  ANewPos := Source.Position + Offset;
  case Origin of
    soFromBeginning:
      ANewPos := PosStart + Offset;
    soFromEnd:
      ANewPos := PosEnd + Offset;
  end;
  Source.Position := Min(Max(PosStart, ANewPos), PosEnd);
  Result := Source.Position - PosStart;
end;

function TdxSkinPartStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Source.Write(Buffer, Count);
  FPosEnd := Source.Position;
end;

{ TdxSkinRender }

class procedure TdxSkinRender.FillRectByGradient(ACanvas: TdxGPCanvas;
  const R: TRect; AColor1, AColor2: TColor; AMode: TdxSkinGradientMode; AAlpha: Byte = 255);
begin
  if cxColorIsValid(AColor1) then
  begin
    if (AColor1 = AColor2) or not cxColorIsValid(AColor2) then
      ACanvas.FillRectangle(R, dxColorToAlphaColor(AColor1, AAlpha))
    else
      ACanvas.FillRectangleByGradient(R,
        dxColorToAlphaColor(AColor1, AAlpha),
        dxColorToAlphaColor(AColor2, AAlpha),
        dxSkinsGradientModeMap[AMode]);
  end;
end;

{ TdxSkinElementCache }

constructor TdxSkinElementCache.Create;
begin
  FScaleFactor := TdxScaleFactor.Create;
end;

destructor TdxSkinElementCache.Destroy;
begin
  FreeCache;
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TdxSkinElementCache.FreeCache;
begin
  if Assigned(FCache) then
  begin
    GdipCheck(GdipDisposeImage(FCache));
    FCache := nil;
  end;
  FreeAndNil(FCacheOpaque);
end;

procedure TdxSkinElementCache.CheckCacheState(AElement: TdxSkinElement;
  const R: TRect; AState: TdxSkinElementState; AImageIndex: Integer);
begin
  CheckCacheState(AElement, R, dxDefaultScaleFactor, AState, AImageIndex);
end;

procedure TdxSkinElementCache.CheckCacheState(
  AElement: TdxSkinElement; const R: TRect; AScaleFactor: TdxScaleFactor;
  AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0);
begin
  if (AElement <> Element) or (AState <> FState) or
    (FImageIndex <> AImageIndex) or (FImageSet <> dxSkinsUseImageSet) or
    not cxSizeIsEqual(R, FRect) or not FScaleFactor.Equals(AScaleFactor) then
  begin
    FRect := R;
    FState := AState;
    FElement := AElement;
    FImageIndex := AImageIndex;
    FImageSet := dxSkinsUseImageSet;
    FScaleFactor.Assign(AScaleFactor);
    FreeCache;
    if not IsRectEmpty(R) then
    begin
      if AElement.IsAlphaUsed then
        InitCache(R)
      else
        InitOpaqueCache(R);
    end;
  end;
end;

procedure TdxSkinElementCache.InitCache(R: TRect);
var
  AGraphics: GpGraphics;
begin
  OffsetRect(R, -R.Left, -R.Top);
  FCache := dxGpCreateBitmap(R);
  GdipCheck(GdipGetImageGraphicsContext(FCache, AGraphics));
  try
    dxGPPaintCanvas.BeginPaint(AGraphics);
    try
      Element.DrawEx(dxGPPaintCanvas, R, FScaleFactor, ImageIndex, State);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  finally
    GdipCheck(GdipDeleteGraphics(AGraphics));
  end;
end;

procedure TdxSkinElementCache.InitOpaqueCache(const R: TRect);
begin
  FCacheOpaque := TcxBitmap.CreateSize(cxRectWidth(R), cxRectHeight(R), pf32bit);
  dxGPPaintCanvas.BeginPaint(FCacheOpaque.Canvas.Handle, FCacheOpaque.ClientRect);
  try
    Element.DrawEx(dxGPPaintCanvas, FCacheOpaque.ClientRect, FScaleFactor, ImageIndex, State);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxSkinElementCache.Draw(DC: HDC; const R: TRect);
begin
  if FCacheOpaque <> nil then
    cxBitBlt(DC, FCacheOpaque.Canvas.Handle, R, cxNullPoint, SRCCOPY)
  else
    if FCache <> nil then
    begin
      dxGPPaintCanvas.BeginPaint(DC, R);
      try
        GdipCheck(GdipDrawImageRectI(dxGPPaintCanvas.Handle, FCache, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top));
      finally
        dxGPPaintCanvas.EndPaint;
      end;
    end;
end;

procedure TdxSkinElementCache.Draw(ACanvas: TdxGPCanvas; const R: TRect);
begin
  if FCacheOpaque <> nil then
    ACanvas.DrawBitmap(FCacheOpaque, R)
  else
    if FCache <> nil then
      GdipCheck(GdipDrawImageRectI(ACanvas.Handle, FCache, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top));
end;

procedure TdxSkinElementCache.DrawEx(DC: HDC; AElement: TdxSkinElement;
  const R: TRect; AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState; AImageIndex: Integer);
begin
  CheckCacheState(AElement, R, AScaleFactor, AState, AImageIndex);
  Draw(DC, R);
end;

procedure TdxSkinElementCache.DrawEx(ACanvas: TdxGPCanvas; AElement: TdxSkinElement; const R: TRect;
  AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0);
begin
  CheckCacheState(AElement, R, AScaleFactor, AState, AImageIndex);
  Draw(ACanvas, R);
end;

procedure TdxSkinElementCache.DrawEx(DC: HDC; AElement: TdxSkinElement;
  const R: TRect; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0);
begin
  DrawEx(DC, AElement, R, dxDefaultScaleFactor, AState, AImageIndex);
end;

procedure TdxSkinElementCache.Flush;
begin
  FElement := nil;
  FScaleFactor.Assign(1, 1);
  FRect := cxNullRect;
  FreeCache;
end;

{ TdxSkinElementCacheList }

constructor TdxSkinElementCacheList.Create(ACapacity: Integer = DefaultCapacity);
begin
  inherited Create;
  FData := TObjectList.Create(True);
  FData.Capacity := ACapacity;
  Capacity := ACapacity;
end;

destructor TdxSkinElementCacheList.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TdxSkinElementCacheList.DrawElement(DC: HDC; AElement: TdxSkinElement;
  const R: TRect; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0);
begin
  DrawElement(DC, AElement, R, dxSystemScaleFactor, AState, AImageIndex);
end;

procedure TdxSkinElementCacheList.DrawElement(DC: HDC; AElement: TdxSkinElement; const R: TRect;
  AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0);
begin
  GetCacheItem(AElement, R, AState, AImageIndex).DrawEx(DC, AElement, R, AScaleFactor, AState, AImageIndex);
end;

procedure TdxSkinElementCacheList.DrawElement(ACanvas: TdxGPCanvas; AElement: TdxSkinElement; const R: TRect;
  AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0);
begin
  GetCacheItem(AElement, R, AState, AImageIndex).DrawEx(ACanvas, AElement, R, AScaleFactor, AState, AImageIndex);
end;

procedure TdxSkinElementCacheList.CheckCapacity;
begin
  while FData.Count > Capacity do
    FData.Delete(0);
end;

procedure TdxSkinElementCacheList.Flush;
begin
  FData.Count := 0;
end;

function TdxSkinElementCacheList.GetCacheItem(AElement: TdxSkinElement;
  const R: TRect; AState: TdxSkinElementState; AImageIndex: Integer): TdxSkinElementCache;
begin
  if not TryGet(AElement, R, AState, AImageIndex, Result) then
  begin
    if FData.Count = Capacity then
    begin
      FData.Move(0, FData.Count - 1);
      Result := TdxSkinElementCache(FData.Last);
    end
    else
    begin
      Result := TdxSkinElementCache.Create;
      FData.Add(Result);
    end;
  end;
end;

function TdxSkinElementCacheList.TryGet(AElement: TdxSkinElement; const R: TRect;
  AState: TdxSkinElementState; AImageIndex: Integer; out AElementCache: TdxSkinElementCache): Boolean;
var
  I: Integer;
begin
  for I := 0 to FData.Count - 1 do
  begin
    AElementCache := TdxSkinElementCache(FData.List[I]);
    if (AElementCache.Element = AElement) and (AImageIndex = AElementCache.ImageIndex) and
      (AState = AElementCache.State) and cxSizeIsEqual(R, AElementCache.Rect)
    then
      Exit(True);
  end;

  for I := 0 to FData.Count - 1 do
  begin
    AElementCache := TdxSkinElementCache(FData.List[I]);
    if (AElementCache.Element = AElement) and cxSizeIsEqual(R, AElementCache.Rect) then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxSkinElementCacheList.SetCapacity(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if FCapacity <> AValue then
  begin
    FCapacity := AValue;
    CheckCapacity;
  end;
end;

{ TdxSkinBinaryWriter }

constructor TdxSkinBinaryWriter.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FHeaderOffset := AStream.Position;
  WriteHeader;
end;

destructor TdxSkinBinaryWriter.Destroy;
begin
  WriteHeader;
  inherited Destroy;
end;

procedure TdxSkinBinaryWriter.AddSkin(ASkin: TdxSkin);
var
  ASavedPosition: Integer;
  ASkinSize: Integer;
begin
  Inc(FCount);
  ASavedPosition := Stream.Position;
  Stream.WriteBuffer(ASkinSize, SizeOf(ASkinSize));
  ASkin.SaveToStream(Stream);
  ASkinSize := Stream.Position - ASavedPosition - SizeOf(ASkinSize);
  Stream.Position := ASavedPosition;
  Stream.WriteBuffer(ASkinSize, SizeOf(ASkinSize));
  Stream.Position := ASavedPosition + ASkinSize + SizeOf(ASkinSize);
end;

procedure TdxSkinBinaryWriter.WriteHeader;
begin
  Stream.Position := FHeaderOffset;
  dxSkinWriteSignature(Stream);
  Stream.WriteBuffer(FCount, SizeOf(FCount));
end;

{ TdxSkinBinaryReader }

constructor TdxSkinBinaryReader.Create(AStream: TStream);
begin
  inherited Create;
  FSkins := TcxObjectList.Create;
  FStream := AStream;
  ReadSkinsInfo;
end;

destructor TdxSkinBinaryReader.Destroy;
begin
  FreeAndNil(FSkins);
  inherited Destroy;
end;

function TdxSkinBinaryReader.IndexOf(const ASkinName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if SameText(ASkinName, SkinName[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxSkinBinaryReader.LoadSkin(ASkin: TdxSkin; ASkinIndex: Integer): Boolean;
begin
  Result := (ASkinIndex >= 0) and (ASkinIndex < Count);
  if Result then
  begin
    Stream.Position := SkinOffset[ASkinIndex];
    ASkin.LoadFromStream(Stream);
  end;
end;

function TdxSkinBinaryReader.LoadSkin(ASkin: TdxSkin; const ASkinName: string): Boolean;
begin
  if ASkinName <> '' then
    Result := LoadSkin(ASkin, IndexOf(ASkinName))
  else
    Result := LoadSkin(ASkin, 0);
end;

function TdxSkinBinaryReader.ReadBinaryProject(AStream: TStream): Boolean;
var
  ASavedPosition: Integer;
  ASkinCount: Integer;
  ASkinSize: Integer;
  AVersion: Double;
  I: Integer;
begin
  Result := False;
  if dxSkinCheckSignature(AStream, AVersion) then
  begin
    if AStream.Read(ASkinCount, SizeOf(ASkinCount)) = SizeOf(ASkinCount) then
    begin
      for I := 0 to ASkinCount - 1 do
      begin
        AStream.ReadBuffer(ASkinSize, SizeOf(ASkinSize));
        ASavedPosition := AStream.Position;
        if not ReadBinarySkin(AStream) then Exit;
        AStream.Position := ASavedPosition + ASkinSize;
      end;
      Result := True;
    end;
  end;
end;

function TdxSkinBinaryReader.ReadBinarySkin(AStream: TStream): Boolean;
var
  ADetails: TdxSkinDetails;
begin
  ADetails := TdxSkinDetails.Create;
  Result := ADetails.LoadFromStream(AStream);
  if Result then
    FSkins.Add(ADetails)
  else
    ADetails.Free;
end;

procedure TdxSkinBinaryReader.ReadSkinsInfo;
var
  ASavedPosition: Integer;
begin
  ASavedPosition := Stream.Position;
  if not ReadBinaryProject(Stream) then
  begin
    Stream.Position := ASavedPosition;
    ReadBinarySkin(Stream);
  end;
end;

function TdxSkinBinaryReader.GetCount: Integer;
begin
  Result := FSkins.Count;
end;

function TdxSkinBinaryReader.GetSkinDetails(Index: Integer): TdxSkinDetails;
begin
  Result := TdxSkinDetails(FSkins[Index]);
end;

function TdxSkinBinaryReader.GetSkinDisplayName(Index: Integer): string;
begin
  Result := SkinDetails[Index].DisplayName;
end;

function TdxSkinBinaryReader.GetSkinName(Index: Integer): string;
begin
  Result := SkinDetails[Index].Name;
end;

function TdxSkinBinaryReader.GetSkinOffset(Index: Integer): Integer;
begin
  Result := SkinDetails[Index].FDataOffset;
end;

{ TdxSkinDetails }

constructor TdxSkinDetails.Create;
var
  ASize: TdxSkinIconSize;
begin
  inherited Create;
  for ASize := Low(TdxSkinIconSize) to High(TdxSkinIconSize) do
  begin
    FIcons[ASize] := TdxSmartImage.Create;
    FIcons[ASize].OnChange := DoIconsChanged;
  end;
end;

destructor TdxSkinDetails.Destroy;
var
  ASize: TdxSkinIconSize;
begin
  for ASize := Low(TdxSkinIconSize) to High(TdxSkinIconSize) do
  begin
    FIcons[ASize].OnChange := nil;
    FreeAndNil(FIcons[ASize]);
  end;
  inherited Destroy;
end;

procedure TdxSkinDetails.Assign(Source: TPersistent);
var
  ASize: TdxSkinIconSize;
begin
  if Source is TdxSkinDetails then
  begin
    BeginUpdate;
    try
      Name := TdxSkinDetails(Source).Name;
      Notes := TdxSkinDetails(Source).Notes;
      GroupName := TdxSkinDetails(Source).GroupName;
      DisplayName := TdxSkinDetails(Source).DisplayName;
      for ASize := Low(TdxSkinIconSize) to High(TdxSkinIconSize) do
        Icons[ASize].Assign(TdxSkinDetails(Source).Icons[ASize]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSkinDetails.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxSkinDetails.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TdxSkinDetails.Changed;
begin
  if FUpdateCount = 0 then
  begin
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TdxSkinDetails.Clear;
var
  ASize: TdxSkinIconSize;
begin
  BeginUpdate;
  try
    Name := '';
    Notes := '';
    GroupName := '';
    DisplayName := '';
    for ASize := Low(TdxSkinIconSize) to High(TdxSkinIconSize) do
      Icons[ASize].Clear;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinDetails.DataRead(AStream: TStream; const AVersion: TdxSkinVersion);
var
  ASize: TdxSkinIconSize;
begin
  BeginUpdate;
  try
    Clear;
    FName := dxSkinReadStringFromStream(AStream);
    if AVersion >= 1.04 then
    begin
      FDisplayName := dxSkinReadStringFromStream(AStream);
      FGroupName := dxSkinReadStringFromStream(AStream);
      FNotes := dxSkinReadWideStringFromStream(AStream);
      dxSkinReadSmartImageFromStream(AStream, Icons[sis16]);
      dxSkinReadSmartImageFromStream(AStream, Icons[sis48]);
    end;
    for ASize := Low(TdxSkinIconSize) to High(TdxSkinIconSize) do
    begin
      if Icons[ASize].Empty then
        ResetIcon(ASize);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinDetails.DataWrite(AStream: TStream);
begin
  dxSkinWriteStringToStream(AStream, Name);
  dxSkinWriteStringToStream(AStream, DisplayName);
  dxSkinWriteStringToStream(AStream, GroupName);
  dxSkinWriteWideStringToStream(AStream, Notes);
  dxSkinWriteSmartImageToStream(AStream, Icons[sis16]);
  dxSkinWriteSmartImageToStream(AStream, Icons[sis48]);
end;

procedure TdxSkinDetails.DoIconsChanged(Sender: TObject);
begin
  Changed;
end;

function TdxSkinDetails.LoadFromStream(AStream: TStream): Boolean;
var
  ASkin: TdxSkin;
  AVersion: TdxSkinVersion;
begin
  FDataOffset := AStream.Position;
  Result := dxSkinCheckSignature(AStream, AVersion);
  if Result then
  begin
    if AVersion < 1.04 then
    begin
      ASkin := TdxSkin.Create('', False, 0);
      try
        AStream.Position := FDataOffset;
        ASkin.LoadFromStream(AStream);
        Assign(ASkin.Details);
      finally
        ASkin.Free;
      end;
    end
    else
      DataRead(AStream, AVersion);
  end;
end;

procedure TdxSkinDetails.ResetIcon(ASize: TdxSkinIconSize);
const
  SkinIconNames: array[TdxSkinIconSize] of string =
    (sdxSkinDefaultSkinIcon, sdxSkinDefaultSkinIconLarge);
begin
  Icons[ASize].LoadFromResource(HInstance, SkinIconNames[ASize], sdxResourceType);
end;

function TdxSkinDetails.GetIcon(ASize: TdxSkinIconSize): TdxSmartImage;
begin
  Result := FIcons[ASize];
end;

procedure TdxSkinDetails.SetDisplayName(const AValue: string);
begin
  if AValue <> FDisplayName then
  begin
    FDisplayName := AValue;
    Changed;
  end;
end;

procedure TdxSkinDetails.SetGroupName(const AValue: string);
begin
  if AValue <> FGroupName then
  begin
    FGroupName := AValue;
    Changed;
  end;
end;

procedure TdxSkinDetails.SetName(const AValue: string);
begin
  if AValue <> FName then
  begin
    FName := AValue;
    Changed;
  end;
end;

procedure TdxSkinDetails.SetNotes(const AValue: WideString);
begin
  if AValue <> FNotes then
  begin
    FNotes := AValue;
    Changed;
  end;
end;

{ TdxSkinCustomListProperty }

constructor TdxSkinCustomListProperty.Create(AOwner: TPersistent; const AName: string);
begin
  inherited;
  FProperties := CreateProperties;
end;

destructor TdxSkinCustomListProperty.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

procedure TdxSkinCustomListProperty.BeforeDestruction;
begin
  inherited;
  FProperties.Clear;
end;

procedure TdxSkinCustomListProperty.Clear;
begin
  FProperties.Clear;
end;

function TdxSkinCustomListProperty.Compare(AProperty: TdxSkinProperty): Boolean;
begin
  Result := inherited Compare(AProperty) and (AProperty is TdxSkinCustomListProperty) and
    FProperties.Compare(TdxSkinCustomListProperty(AProperty).FProperties);
end;

procedure TdxSkinCustomListProperty.Delete(const AProperty: TdxSkinProperty);
begin
  BeginUpdate;
  try
    FProperties.FreeAndRemove(AProperty);
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinCustomListProperty.Delete(const AIndex: Integer);
begin
  BeginUpdate;
  try
    FProperties.FreeAndDelete(AIndex);
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinCustomListProperty.Delete(const AName: string);
var
  AProperty: TdxSkinProperty;
begin
  if Find(AName, AProperty) then
    Delete(AProperty);
end;

function TdxSkinCustomListProperty.Find(const AName: string; var AProperty): Boolean;
begin
  Result := FProperties.Find(AName, AProperty);
end;

procedure TdxSkinCustomListProperty.Sort(ASortChildren: Boolean = False);
begin
  FProperties.Sort(ASortChildren);
end;

procedure TdxSkinCustomListProperty.AssignCore(Source: TPersistent);
begin
  if Source is TdxSkinCustomListProperty then
    FProperties.Assign(TdxSkinCustomListProperty(Source).FProperties);
end;

function TdxSkinCustomListProperty.CreateProperties: TdxSkinProperties;
begin
  Result := TdxSkinProperties.CreateEx(Self, HandlerPropertiesChanged);
end;

procedure TdxSkinCustomListProperty.DataRead(Stream: TStream; const AVersion: Double);
begin
  inherited;
  FProperties.DataRead(Stream, AVersion);
end;

procedure TdxSkinCustomListProperty.DataWrite(Stream: TStream);
begin
  inherited;
  FProperties.DataWrite(Stream);
end;

procedure TdxSkinCustomListProperty.FlushCache;
begin
  inherited;
  FProperties.FlushCache;
end;

function TdxSkinCustomListProperty.GetCount: Integer;
begin
  Result := FProperties.Count;
end;

function TdxSkinCustomListProperty.GetItem(Index: Integer): TdxSkinProperty;
begin
  Result := FProperties.List[Index];
end;

procedure TdxSkinCustomListProperty.HandlerPropertiesChanged(Sender: TObject; Changes: TdxSkinChanges);
begin
  Changed(Changes);
end;

{ TdxSkinColorPalette }

constructor TdxSkinColorPalette.Create(AOwner: TPersistent; const AName: string);
begin
  inherited;
  if AOwner is TdxSkinColorPalettes then
    FDefaultColorClass := TdxSkinColorPalettes(AOwner).FDefaultColorClass
  else
    FDefaultColorClass := TdxSkinColor;
end;

function TdxSkinColorPalette.Add(const AName: string; AValue: TColor): TdxSkinColor;
begin
  Result := AddCore(AName, AValue, '');
end;

function TdxSkinColorPalette.Add(const AName, AValueReference: string): TdxSkinColor;
begin
  Result := AddCore(AName, clDefault, AValueReference);
end;

function TdxSkinColorPalette.AddCore(const AName: string; AValue: TColor; AValueReference: string): TdxSkinColor;
begin
  BeginUpdate;
  try
    Result := TdxSkinColor(FProperties.Add(AName, FDefaultColorClass));
    Result.Value := AValue;
    Result.ValueReference := AValueReference;
  finally
    EndUpdate;
  end;
end;

procedure TdxSkinColorPalette.DoChanged(AChanges: TdxSkinChanges);
begin
  inherited;
  FlushCache;
end;

procedure TdxSkinColorPalette.FlushCache;
begin
  inherited FlushCache;
  ZeroMemory(@FID, SizeOf(FID));
end;

function TdxSkinColorPalette.GetID: TGUID;
begin
  if FID.D1 = 0 then
  begin
    CreateGUID(FID);
    if FID.D1 = 0 then
      FID.D1 := MaxWord;
  end;
  Result := FID;
end;

function TdxSkinColorPalette.GetFillColor(const ID: string): TdxAlphaColor;
var
  AProperty: TdxSkinProperty;
begin
  if Find(ID, AProperty) then
    Result := TdxSkinColor(AProperty).ValueAsAlphaColor
  else
    Result := TdxAlphaColors.Default;
end;

function TdxSkinColorPalette.GetStrokeColor(const ID: string): TdxAlphaColor;
begin
  Result := GetFillColor(ID);
end;

function TdxSkinColorPalette.GetItem(Index: Integer): TdxSkinColor;
begin
  Result := FProperties.List[Index];
end;

{ TdxSkinColorPalettes }

constructor TdxSkinColorPalettes.Create(AOwner: TPersistent; const AName: string);
begin
  inherited;
  FDefaultColorClass := TdxSkinColor;
end;

function TdxSkinColorPalettes.Add(const AName: string): TdxSkinColorPalette;
begin
  Result := TdxSkinColorPalette(FProperties.Add(AName, TdxSkinColorPalette));
end;

function TdxSkinColorPalettes.GetItem(Index: Integer): TdxSkinColorPalette;
begin
  Result := TdxSkinColorPalette(FProperties.List[Index]);
end;

{ TdxSkinDefaultColorPalette }

procedure TdxSkinGeneralColorPalette.AssignCore(Source: TPersistent);
begin
  inherited;
  if Source is TdxSkinColorPalette then
    Name := TdxSkinColorPalette(Source).Name;
end;

//

procedure RegisterAssistants;
begin
  FSkinEmptyElement := TdxSkinEmptyElement.Create(nil, '');
  RegisterClasses([TdxSkinControlGroup, TdxSkinElement, TdxSkinImage, TdxSkinColorPalette, TdxSkinAlphaColor]);

  TdxSkinAlternateImageAttributes.Register;
  TdxSkinBooleanProperty.Register;
  TdxSkinColor.Register;
  TdxSkinColorPalettes.Register;
  TdxSkinIntegerProperty.Register;
  TdxSkinRectProperty.Register;
  TdxSkinSizeProperty.Register;
  TdxSkinStringProperty.Register;
  TdxSkinWideStringProperty.Register;

  CheckGdiPlus;
  CheckPngCodec;
end;

procedure UnregisterAssistants;
begin
  UnregisterClasses([TdxSkinControlGroup, TdxSkinElement, TdxSkinImage, TdxSkinColorPalette, TdxSkinAlphaColor]);

  TdxSkinAlternateImageAttributes.Unregister;
  TdxSkinBooleanProperty.Unregister;
  TdxSkinColor.Unregister;
  TdxSkinColorPalettes.Unregister;
  TdxSkinIntegerProperty.Unregister;
  TdxSkinRectProperty.Unregister;
  TdxSkinSizeProperty.Unregister;
  TdxSkinStringProperty.Unregister;
  TdxSkinWideStringProperty.Unregister;

  FreeAndNil(FSkinEmptyElement);
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);

end.
