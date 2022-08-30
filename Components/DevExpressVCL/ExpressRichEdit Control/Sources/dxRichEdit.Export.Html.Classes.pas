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

unit dxRichEdit.Export.Html.Classes;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Rtti, RegularExpressions,
  dxCore, dxCoreClasses, dxCoreGraphics, dxCultureInfo, dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Export.Html.Types,
  dxRichEdit.Export.Html.Utils;

type
  TdxHtmlTextWriter = class;
  TdxCssTextWriter = class;
  TdxWebControlBase = class;
  TdxWebControlCollection = class;
  TdxWebStyle = class;

  { TdxSimpleBitVector32 }

  TdxSimpleBitVector32 = record
  strict private
    FValue: Cardinal;
    function GetItem(const Value: Cardinal): Boolean;
  public
    class function Create: TdxSimpleBitVector32; static;
    procedure &Set(const Value: Cardinal);
    procedure Clear(const Value: Cardinal);

    property Items[const Value: Cardinal]: Boolean read GetItem; default;
  end;

  { TdxWebUnit }

  TdxWebUnit = record
  public const
    MaxValue = 32767;
    MinValue = -32768;
  private
{$IFDEF DELPHIXE3}
    class var
      FEmpty: TdxWebUnit;
{$ELSE}
    class function GetEmpty: TdxWebUnit; static;
{$ENDIF}
  private
    FType: TdxWebUnitType;
    FValue: Double;
    class constructor Initialize;
    class function GetStringFromType(AType: TdxWebUnitType): string; static;
    class function GetTypeFromString(const AValue: string): TdxWebUnitType; static;
    function GetIsEmpty: Boolean;
    function GetType: TdxWebUnitType;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create(AValue: Double); overload;
    constructor Create(AValue: Double; AType: TdxWebUnitType); overload;
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: string; const ACulture: TdxCultureInfo); overload;
    constructor Create(const AValue: string; const ACulture: TdxCultureInfo; ADefaultType: TdxWebUnitType); overload;

    class operator Equal(const A, B: TdxWebUnit): Boolean;
    class operator NotEqual(const A, B: TdxWebUnit): Boolean;

    class function Parse(const S: string): TdxWebUnit; overload; static;
    class function Parse(const S: string; const ACulture: TdxCultureInfo): TdxWebUnit; overload; static;
    class function Percentage(N: Double): TdxWebUnit; static;
    class function Pixel(N: Integer): TdxWebUnit; static;
    class function Point(N: Integer): TdxWebUnit; static;
    function ToString: string; overload;
    function ToString(const ACulture: TdxCultureInfo): string; overload;

    property IsEmpty: Boolean read GetIsEmpty;
    property &Type: TdxWebUnitType read GetType;
    property Value: Double read FValue;
    class property Empty: TdxWebUnit read {$IFDEF DELPHIXE3}FEmpty{$ELSE}GetEmpty{$ENDIF};
  end;

  { TdxWebFontUnit }

  TdxWebFontUnit = record
  private
{$IFDEF DELPHIXE3}
    class var
      FEmpty: TdxWebFontUnit;
      FSmaller: TdxWebFontUnit;
      FLarger: TdxWebFontUnit;
      FXXSmall: TdxWebFontUnit;
      FXSmall: TdxWebFontUnit;
      FSmall: TdxWebFontUnit;
      FMedium: TdxWebFontUnit;
      FLarge: TdxWebFontUnit;
      FXLarge: TdxWebFontUnit;
      FXXLarge: TdxWebFontUnit;
{$ELSE}
    class function GetEmpty: TdxWebFontUnit; static;
    class function GetSmaller: TdxWebFontUnit; static;
    class function GetLarger: TdxWebFontUnit; static;
    class function GetXXSmall: TdxWebFontUnit; static;
    class function GetXSmall: TdxWebFontUnit; static;
    class function GetSmall: TdxWebFontUnit; static;
    class function GetMedium: TdxWebFontUnit; static;
    class function GetLarge: TdxWebFontUnit; static;
    class function GetXLarge: TdxWebFontUnit; static;
    class function GetXXLarge: TdxWebFontUnit; static;
{$ENDIF}
    class constructor Initialize;
  private
    FType: TdxWebFontSize;
    FValue: TdxWebUnit;
    function GetIsEmpty: Boolean;
  public
    constructor Create(AType: TdxWebFontSize); overload;
    constructor Create(AValue: TdxWebUnit); overload;
    constructor Create(AValue: Integer); overload;
    constructor Create(AValue: Double); overload;
    constructor Create(AValue: Double; AType: TdxWebUnitType); overload;
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: string; const ACulture: TdxCultureInfo); overload;

    class operator Equal(const A, B: TdxWebFontUnit): Boolean;
    class operator NotEqual(const A, B: TdxWebFontUnit): Boolean;

    class function Parse(const S: string): TdxWebFontUnit; overload; static;
    class function Parse(const S: string; const ACulture: TdxCultureInfo): TdxWebFontUnit; overload; static;
    class function Point(N: Integer): TdxWebFontUnit; static;
    function ToString: string; overload;
    function ToString(const ACulture: TdxCultureInfo): string; overload;

    property IsEmpty: Boolean read GetIsEmpty;
    property &Type: TdxWebFontSize read FType;
    property &Unit: TdxWebUnit read FValue;

    class property Empty: TdxWebFontUnit read {$IFDEF DELPHIXE3}FEmpty{$ELSE}GetEmpty{$ENDIF};
    class property Smaller: TdxWebFontUnit read {$IFDEF DELPHIXE3}FSmaller{$ELSE}GetSmaller{$ENDIF};
    class property Larger: TdxWebFontUnit read {$IFDEF DELPHIXE3}FLarger{$ELSE}GetLarger{$ENDIF};
    class property XXSmall: TdxWebFontUnit read {$IFDEF DELPHIXE3}FXXSmall{$ELSE}GetXXSmall{$ENDIF};
    class property XSmall: TdxWebFontUnit read {$IFDEF DELPHIXE3}FXSmall{$ELSE}GetXSmall{$ENDIF};
    class property Small: TdxWebFontUnit read {$IFDEF DELPHIXE3}FSmall{$ELSE}GetSmall{$ENDIF};
    class property Medium: TdxWebFontUnit read {$IFDEF DELPHIXE3}FMedium{$ELSE}GetMedium{$ENDIF};
    class property Large: TdxWebFontUnit read {$IFDEF DELPHIXE3}FLarge{$ELSE}GetLarge{$ENDIF};
    class property XLarge: TdxWebFontUnit read {$IFDEF DELPHIXE3}FXLarge{$ELSE}GetXLarge{$ENDIF};
    class property XXLarge: TdxWebFontUnit read {$IFDEF DELPHIXE3}FXXLarge{$ELSE}GetXXLarge{$ENDIF};
  end;

  { TdxWebFontInfo }

  TdxWebFontInfo = class
  strict private
    FOwner: TdxWebStyle;
    procedure ResetBold;
    procedure ResetFontSize;
    procedure ResetItalic;
    procedure ResetNames;
    procedure ResetOverline;
    procedure ResetStrikeout;
    procedure ResetUnderline;
    function GetBold: Boolean;
    procedure SetBold(const AValue: Boolean);
    function GetItalic: Boolean;
    procedure SetItalic(const AValue: Boolean);
    function GetName: string;
    procedure SetName(const AValue: string);
    function GetNames: TArray<string>;
    procedure SetNames(const AValue: TArray<string>);
    function GetOverline: Boolean;
    procedure SetOverline(const AValue: Boolean);
    function GetSize: TdxWebFontUnit;
    procedure SetSize(const AValue: TdxWebFontUnit);
    function GetStrikeout: Boolean;
    procedure SetStrikeout(const AValue: Boolean);
    function GetUnderline: Boolean;
    procedure SetUnderline(const AValue: Boolean);
  public
    constructor Create(AOwner: TdxWebStyle);
    procedure ClearDefaults;
    procedure CopyFrom(const F: TdxWebFontInfo);
    procedure MergeWith(const F: TdxWebFontInfo);
    procedure Reset;
    function ToString: string; override;

    property Bold: Boolean read GetBold write SetBold;
    property Italic: Boolean read GetItalic write SetItalic;
    property Name: string read GetName write SetName;
    property Names: TArray<string> read GetNames write SetNames;
    property Overline: Boolean read GetOverline write SetOverline;
    property Owner: TdxWebStyle read FOwner;
    property Size: TdxWebFontUnit read GetSize write SetSize;
    property Strikeout: Boolean read GetStrikeout write SetStrikeout;
    property Underline: Boolean read GetUnderline write SetUnderline;
  end;

  { TdxWebControlBase }

  TdxWebControlBase = class(TcxIUnknownObject)
  strict private
    FCachedUniqueID: string;
    FControls: TdxWebControlCollection;
    FControlState: TdxWebControlState;
    FId: string;
    FNamingContainer: TdxWebControlBase;
    FNamedControls: TdxNamedOrdinalDictionary<TdxWebControlBase>;
    FNamedControlsID: Integer;
    FParent: TdxWebControlBase;
    FViewState: TdxStateBag;
    FUniqueIDPrefix: string;
    FFlags: TdxSimpleBitVector32;
    function GetChildControlsCreated: Boolean;
    function GetIdSeparator: Char;
    function GetIsChildControlStateCleared: Boolean;
    function GetIsTrackingViewState: Boolean;
    procedure SetChildControlsCreated(const AValue: Boolean);
  protected
    function CreateControlCollection: TdxWebControlCollection; virtual;
    function FindControl(const AId: string; APathOffset: Integer): TdxWebControlBase; overload; virtual;
    function GetClientID: string; virtual;
    function GetControls: TdxWebControlCollection; virtual;
    function GetEnableTheming: Boolean; virtual;
    function GetID: string; virtual;
    function GetNamingContainer: TdxWebControlBase; virtual;
    function GetParent: TdxWebControlBase; virtual;
    function GetUniqueID: string; virtual;
    function GetViewState: TdxStateBag; virtual;
    function GetVisible: Boolean; virtual;
    function IsLiteral: Boolean; virtual;
    function IsLiteralContent: Boolean; virtual;
    function OnBubbleEvent(ASource: TObject; AArgs: TdxEventArgs): Boolean; virtual;
    procedure AddedControl(AControl: TdxWebControlBase; AIndex: Integer); virtual;
    procedure BuildProfileTree(ACalcViewState: Boolean);
    procedure ClearCachedUniqueIDRecursive;
    procedure CreateChildControls; virtual;
    procedure EnsureChildControls; virtual;
    procedure EnsureID;
    procedure EnsureNamedControlsTable;
    procedure FillNamedControlsTable(ANamingContainer: TdxWebControlBase; AControls: TdxWebControlCollection);
    procedure GenerateAutomaticID;
    procedure LoadControlState(ASavedState: TObject); virtual;
    procedure OnDataBinding(E: TdxEventArgs); virtual;
    procedure OnInit(E: TdxEventArgs); virtual;
    procedure OnLoad(E: TdxEventArgs); virtual;
    procedure OnPreRender(E: TdxEventArgs); virtual;
    procedure OnUnload(E: TdxEventArgs); virtual;
    procedure RaiseBubbleEvent(ASource: TObject; AArgs: TdxEventArgs);
    procedure Render(AWriter: TdxHtmlTextWriter); virtual;
    procedure RenderChildren(AWriter: TdxHtmlTextWriter); virtual;
    procedure RenderControlInternal(AWriter: TdxHtmlTextWriter);
    procedure SetEnableTheming(const AValue: Boolean); virtual;
    procedure SetID(const AValue: string); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
    procedure UpdateNamingContainer(ANamingContainer: TdxWebControlBase);

    property ChildControlsCreated: Boolean read GetChildControlsCreated write SetChildControlsCreated;
    property Flags: TdxSimpleBitVector32 read FFlags;
    property IdSeparator: Char read GetIdSeparator;
    property IsChildControlStateCleared: Boolean read GetIsChildControlStateCleared;
    property IsTrackingViewState: Boolean read GetIsTrackingViewState;
    property ViewState: TdxStateBag read GetViewState;
  public
    constructor Create;
    destructor Destroy; override;

    function FindControl(const AId: string): TdxWebControlBase; overload; virtual;
    function GetUniqueIDPrefix: string; virtual;
    function HasControls: Boolean; virtual;
    function IsDescendentOf(AAncestor: TdxWebControlBase): Boolean;
    function ResolveClientUrl(const ARelativeUrl: string): string;
    procedure ClearNamingContainer;
    procedure DirtyNameTable;
    procedure InitRecursive(ANamingContainer: TdxWebControlBase); virtual;
    procedure LoadRecursive; virtual;
    procedure PreRenderRecursiveInternal; virtual;
    procedure PreventAutoID;
    procedure RemovedControl(AControl: TdxWebControlBase); virtual;
    procedure RenderChildrenInternal(AWriter: TdxHtmlTextWriter; const AChildren: TdxWebControlCollection);
    procedure RenderControl(AWriter: TdxHtmlTextWriter); virtual;
    procedure UnloadRecursive(ADispose: Boolean); virtual;

    property ClientID: string read GetClientID;
    property Controls: TdxWebControlCollection read GetControls;
    property EnableTheming: Boolean read GetEnableTheming write SetEnableTheming;
    property ID: string read GetID write SetID;
    property NamingContainer: TdxWebControlBase read GetNamingContainer;
    property Parent: TdxWebControlBase read GetParent;
    property UniqueID: string read GetUniqueID;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  { TdxWebControlCollection }

  TdxWebControlCollection = class
  strict private
    FControls: TdxList<TdxWebControlBase>;
    FOwner: TdxWebControlBase;
    function GetItem(Index: Integer): TdxWebControlBase;
  protected
    function GetCount: Integer; virtual;

    property Owner: TdxWebControlBase read FOwner;
  public
    constructor Create(AOwner: TdxWebControlBase);
    destructor Destroy; override;

    procedure Add(AChild: TdxWebControlBase); virtual;
    procedure AddAt(AIndex: Integer; AChild: TdxWebControlBase); virtual;
    procedure Clear; virtual;
    function Contains(C: TdxWebControlBase): Boolean; virtual;
    function IndexOf(AValue: TdxWebControlBase): Integer; virtual;
    procedure Remove(AValue: TdxWebControlBase); virtual;
    procedure RemoveAt(AIndex: Integer); virtual;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxWebControlBase read GetItem; default;
  end;

  { TdxWebEmptyControlCollection }

  TdxWebEmptyControlCollection = class(TdxWebControlCollection)
  public
    procedure Add(AChild: TdxWebControlBase); override;
    procedure AddAt(AIndex: Integer; AChild: TdxWebControlBase); override;
  end;

  { TdxCssStyleCollection }

  TdxCssStyleCollection = class
  strict private protected const
    StyleKey = 'style';
  strict private class var
    FStyleAttribRegex: TRegex;
    class constructor Initialize;
  strict private
    FHtmlStyleTable: TDictionary<TdxHtmlTextWriterStyle, string>;
    FStyleTable: TdxStringsDictionary;
    FState: TdxStateBag;
    FStyle: string;
    function GetCount: Integer;
    function GetKeys: TArray<string>;
    function GetValue: string;
    procedure SetValue(const AValue: string);
    procedure ParseString;
  public
    constructor Create(AState: TdxStateBag = nil);
    destructor Destroy; override;
    procedure Add(const AKey, AValue: string); overload;
    procedure Add(AKey: TdxHtmlTextWriterStyle; const AValue: string); overload;
    procedure Clear;
    function GetItem(const AKey: string): string; overload;
    function GetItem(const AKey: TdxHtmlTextWriterStyle): string; overload;
    procedure Remove(const AKey: string); overload;
    procedure Remove(AKey: TdxHtmlTextWriterStyle); overload;
    procedure Render(AWriter: TdxCssTextWriter); overload;
    procedure Render(AWriter: TdxHtmlTextWriter); overload;
    function BuildString: string;

    property Count: Integer read GetCount;
    property Items[const AKey: string]: string read GetItem write Add; default;
    property Keys: TArray<string> read GetKeys;
    property Value: string read GetValue write SetValue;
  end;

  { TdxWebAttributeCollection }

  TdxWebAttributeCollection = class sealed
  strict private
    FBag: TdxStateBag;
    FStyleCollection: TdxCssStyleCollection;
    function GetCount: Integer;
    function GetCssStyle: TdxCssStyleCollection;
    function GetItem(const AKey: string): TdxNullableString;
    procedure SetItem(const AKey: string; const Value: TdxNullableString);
  private
    function GetKeys: TArray<string>;
  public
    constructor Create(ABag: TdxStateBag);
    destructor Destroy; override;
    procedure Add(const AKey: string; const Value: TdxNullableString);
    procedure AddAttributes(AWriter: TdxHtmlTextWriter);
    procedure Clear;
    function Equals(O: TObject): Boolean; override;
    procedure Remove(const AKey: string);
    procedure Render(AWriter: TdxHtmlTextWriter);

    property Count: Integer read GetCount;
    property CssStyle: TdxCssStyleCollection read GetCssStyle;
    property Items[const AKey: string]: TdxNullableString read GetItem write SetItem; default;
    property Keys: TArray<string> read GetKeys;
  end;

  { TdxHtmlControl }

  TdxHtmlControl = class abstract(TdxWebControlBase)
  strict private
    FAttributes: TdxWebAttributeCollection;
    FWebControlFlags: TdxSimpleBitVector32;
    FControlStyle: TdxWebStyle;
    FTagKey: TdxHtmlTextWriterTag;
    function GetAttributes: TdxWebAttributeCollection;
    function GetDisabled: Boolean;
    procedure SetDisabled(const AValue: Boolean);
    function GetStyle: TdxCssStyleCollection;
    function GetControlStyleCreated: Boolean;
    function GetControlStyle: TdxWebStyle;
    function GetTagName: string;
  protected
    procedure AddAttributesToRender(AWriter: TdxHtmlTextWriter); virtual;
    function CreateControlCollection: TdxWebControlCollection; override;
    function CreateControlStyle: TdxWebStyle; virtual;
    function GetAttribute(const AName: string): string; virtual;
    function GetBorderStyle: TdxWebBorderStyle; virtual;
    function GetBorderWidth: TdxWebUnit; virtual;
    function GetCssClass: string; virtual;
    function GetEnabled: Boolean; virtual;
    function GetFont: TdxWebFontInfo; virtual;
    function GetHeight: TdxWebUnit; virtual;
    function GetRequiresLegacyRendering: Boolean; virtual;
    function GetToolTip: string; virtual;
    function GetWidth: TdxWebUnit; virtual;
    procedure Render(AWriter: TdxHtmlTextWriter); override;
    procedure RenderBeginTag(AWriter: TdxHtmlTextWriter); virtual;
    procedure SetAttribute(const AName: string; const AValue: string); virtual;
    procedure SetBorderStyle(const AValue: TdxWebBorderStyle); virtual;
    procedure SetBorderWidth(const AValue: TdxWebUnit); virtual;
    procedure SetCssClass(const AValue: string); virtual;
    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetHeight(const AValue: TdxWebUnit); virtual;
    procedure SetToolTip(const AValue: string); virtual;
    procedure SetWidth(const AValue: TdxWebUnit); virtual;
  public
    constructor Create(ATagKey: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span);
    destructor Destroy; override;
    function ToString: string; override;
    procedure AddDisplayInlineBlockIfNeeded(AWriter: TdxHtmlTextWriter);
    class function MapIntegerAttributeToString(N: Integer): string; static;
    class function MapStringAttributeToString(const S: string): string; static;

    property Attributes: TdxWebAttributeCollection read GetAttributes;
    property BorderStyle: TdxWebBorderStyle read GetBorderStyle write SetBorderStyle;
    property BorderWidth: TdxWebUnit read GetBorderWidth write SetBorderWidth;
    property ControlStyle: TdxWebStyle read GetControlStyle;
    property ControlStyleCreated: Boolean read GetControlStyleCreated;
    property CssClass: string read GetCssClass write SetCssClass;
    property Disabled: Boolean read GetDisabled write SetDisabled;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Font: TdxWebFontInfo read GetFont;
    property Height: TdxWebUnit read GetHeight write SetHeight;
    property RequiresLegacyRendering: Boolean read GetRequiresLegacyRendering;
    property Style: TdxCssStyleCollection read GetStyle;
    property TagKey: TdxHtmlTextWriterTag read FTagKey;
    property TagName: string read GetTagName;
    property ToolTip: string read GetToolTip write SetToolTip;
    property Width: TdxWebUnit read GetWidth write SetWidth;
  end;

  { TdxWebStyle }

  TdxWebStyle = class
  strict private const
  {$REGION 'private const'}
    BorderStyles: array[0..9] of string = ('NotSet', 'None', 'Dotted', 'Dashed', 'Solid', 'Double', 'Groove', 'Ridge', 'Inset', 'Outset');
    CssClassStr = 'class';
  {$ENDREGION}
  public type
  {$REGION 'public type'}
    TAssignedValue = (BackColor, BorderColor, BorderStyle, BorderWidth, CssClass,
      ForeColor, Height, Width, FontBold, FontItalic, FontNames, FontOverline,
      FontSize, FontStrikeout, FontUnderline);
    TAssignedValues = set of TAssignedValue;
  {$ENDREGION}
  strict private
    FAssignedValues: TAssignedValues;
    FFontInfo: TdxWebFontInfo;
    FMarked: Boolean;
    FMarkedAssignedValues: TAssignedValues;
    FOwnStateBag: Boolean;
    FRegisteredCssClass: string;
    FStateBag: TdxStateBag;
    class function FormatStringArray(const AArray: TArray<string>; ADelimiter: Char): string; static;
    function GetBackColor: TdxAlphaColor;
    function GetBorderColor: TdxAlphaColor;
    function GetBorderStyle: TdxWebBorderStyle;
    function GetBorderWidth: TdxWebUnit;
    function GetCssClass: string;
    function GetFont: TdxWebFontInfo;
    function GetForeColor: TdxAlphaColor;
    function GetHeight: TdxWebUnit;
    function GetRegisteredCssClass: string;
    function GetViewState: TdxStateBag;
    function GetWidth: TdxWebUnit;
    procedure SetBackColor(const AValue: TdxAlphaColor);
    procedure SetBorderColor(const AValue: TdxAlphaColor);
    procedure SetBorderStyle(const AValue: TdxWebBorderStyle);
    procedure SetBorderWidth(const AValue: TdxWebUnit);
    procedure SetCssClass(const AValue: string);
    procedure SetForeColor(const AValue: TdxAlphaColor);
    procedure SetHeight(const AValue: TdxWebUnit);
    procedure SetWidth(const AValue: TdxWebUnit);
  protected
    function GetIsEmpty: Boolean; virtual;
    procedure FillStyleAttributes(AAttributes: TdxCssStyleCollection); virtual;
    procedure SetBit(AValue: TAssignedValue); virtual;

    property IsTrackingViewState: Boolean read FMarked;
    property ViewState: TdxStateBag read GetViewState;
  public
    constructor Create(ABag: TdxStateBag = nil);
    destructor Destroy; override;

    procedure AddAttributesToRender(AWriter: TdxHtmlTextWriter; AOwner: TdxHtmlControl = nil); virtual;
    procedure CopyFrom(S: TdxWebStyle); virtual;
    function GetStyleAttributes: TdxCssStyleCollection;
    procedure MergeWith(S: TdxWebStyle); virtual;
    procedure Reset; virtual;
    procedure SetRegisteredCssClass(const ACssClass: string);
    function IsSet(AValue: TAssignedValue): Boolean;
    procedure ClearBit(AValue: TAssignedValue);

    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property BorderColor: TdxAlphaColor read GetBorderColor write SetBorderColor;
    property BorderStyle: TdxWebBorderStyle read GetBorderStyle write SetBorderStyle;
    property BorderWidth: TdxWebUnit read GetBorderWidth write SetBorderWidth;
    property CssClass: string read GetCssClass write SetCssClass;
    property Font: TdxWebFontInfo read GetFont;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property Height: TdxWebUnit read GetHeight write SetHeight;
    property IsEmpty: Boolean read GetIsEmpty;
    property RegisteredCssClass: string read GetRegisteredCssClass;
    property Width: TdxWebUnit read GetWidth write SetWidth;
  end;

  { TdxHtmlStyleRender }

  TdxHtmlStyleRender = class
  public
    class function GetHtmlStyle(AFont: TdxGPFont; AForeColor, ABackColor: TdxAlphaColor): string; overload; static;
    class function GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
      ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor): string; overload; static;
    class function GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
      ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor; AUseFontSizeInPixels: Boolean): string; overload; static;
    class procedure GetHtmlStyle(AFont: TdxGPFont; AForeColor, ABackColor: TdxAlphaColor; AStyle: TdxCssStyleCollection); overload; static;
    class procedure GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
      ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor; AStyle: TdxCssStyleCollection); overload; static;
    class procedure GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
      ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor; AUseFontSizeInPixels: Boolean; AStyle: TdxCssStyleCollection); overload; static;
    class function GetHtmlStyle(AFont: TdxGPFont; AForeColor: TdxAlphaColor): string; overload; static;
    class function GetFontHtml(AFont: TdxGPFont): string; overload; static;
    class function GetFontHtmlInPixels(AFont: TdxGPFont): TObject; static;
    class function GetFontHtml(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
      ABold, AItalic, AStrikeout, AUnderline: Boolean; AInPixels: Boolean): string; overload; static;
    class function GetFontHtml(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
      ABold, AItalic, AStrikeout, AUnderline: Boolean): string; overload; static;
    class procedure GetFontHtml(AFont: TdxGPFont; AStyle: TdxCssStyleCollection); overload; static;
    class procedure GetFontHtml(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
      ABold, AItalic, AStrikeout, AUnderline: Boolean; AInPixels: Boolean; AStyle: TdxCssStyleCollection); overload; static;
    class function GetCorrectedFamilyName(const AFamilyName: string): string; static;
    class function GetFontWeight(AIsBold: Boolean): string; static;
    class function GetFontStyle(AIsItalic: Boolean): string; static;
    class function GetTextDecoration(AStrikeout, AUnderline: Boolean): string; static;
    class function GetTextDecorationValue(AStrikeout, AUnderline: Boolean): string; static;
  end;

  { TdxCssTextWriter }

  TdxCssTextWriter = class(TTextWriter)
{$REGION 'class types'}
  protected type
    TAttributeInformation = record
      Name: string;
      IsUrl: Boolean;
      Encode: Boolean;
      constructor Create(const AName: string; AEncode, AIsUrl: Boolean);
    end;
{$ENDREGION}
  strict private
    class var
      FStyleLookup: TdxNamedOrdinalDictionary<TdxHtmlTextWriterStyle>;
      FAttributeNameLookup: array[TdxHtmlTextWriterStyle] of TAttributeInformation;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    class procedure WriteUrlAttribute(AWriter: TTextWriter; const AUrl: string); static;
    class procedure WriteAttribute(AWriter: TTextWriter; AKey: TdxHtmlTextWriterStyle; const AName, AValue: string); overload; static;
  protected
    class procedure WriteAttributes(AWriter: TTextWriter; AStyles: TList<TdxWebRenderStyle>); static;
  public
    FWriter: TTextWriter;
    constructor Create(AWriter: TTextWriter);
    procedure Close; override;
    procedure Flush; override;
    class function GetStyleKey(const AStyleName: string): TdxHtmlTextWriterStyle; static;
    class function GetStyleName(AStyleKey: TdxHtmlTextWriterStyle): string; static;
    class function IsStyleEncoded(AStyleKey: TdxHtmlTextWriterStyle): Boolean; static;
    class procedure RegisterAttribute(const AName: string; AKey: TdxHtmlTextWriterStyle; AEncode: Boolean = False; AIsUrl: Boolean = False); static;
    procedure Write(AValue: Boolean); override;
    procedure Write(AValue: Char); override;
    procedure Write(AValue: Integer); override;
    procedure Write(const ABuffer: TArray<Char>); override;
    procedure Write(AValue: Double); override;
    procedure Write(AValue: Int64); override;
    procedure Write(AValue: TObject); override;
    procedure Write(AValue: Single); override;
    procedure Write(const S: string); override;
    procedure Write(const ABuffer: TArray<Char>; AIndex: Integer; ACount: Integer); override;
    procedure WriteAttribute(const AName: string; const AValue: string); overload;
    procedure WriteAttribute(AKey: TdxHtmlTextWriterStyle; const AValue: string); overload;
    procedure WriteBeginCssRule(const ASelector: string);
    procedure WriteEndCssRule;
    procedure WriteLine; override;
    procedure WriteLine(AValue: Boolean); override;
    procedure WriteLine(AValue: Char); override;
    procedure WriteLine(AValue: Double); override;
    procedure WriteLine(const ABuffer: TArray<Char>); override;
    procedure WriteLine(AValue: Integer); override;
    procedure WriteLine(AValue: Int64); override;
    procedure WriteLine(AValue: TObject); override;
    procedure WriteLine(AValue: Single); override;
    procedure WriteLine(const S: string); override;
    procedure WriteLine(AValue: Cardinal); override;
    procedure WriteLine(const ABuffer: TArray<Char>; AIndex: Integer; ACount: Integer); override;
  end;

  { TdxHtmlTextWriter }

  TdxHtmlTextWriter = class(TTextWriter)
{$REGION 'class constants'}
  public const
    DefaultTabString            = #9;
    DesignerRegionAttributeName = '_designerRegion';
    DoubleQuoteChar             = '"';
    EndTagLeftChars             = '</';
    EqualsChar                  = '=';
    EqualsDoubleQuoteString     = '="';
    SelfClosingChars            = ' /';
    SelfClosingTagEnd           = ' />';
    SemicolonChar               = ';';
    SingleQuoteChar             = #$27;
    SlashChar                   = '/';
    SpaceChar                   = ' ';
    StyleEqualsChar             = ':';
    TagLeftChar                 = '<';
    TagRightChar                = '>';
{$ENDREGION}
{$REGION 'class types'}
  protected type
    TTagType = (&Inline, NonClosing, Other);

    TTagStackEntry = record
      TagKey: TdxHtmlTextWriterTag;
      EndTagText: string;
      property ToString: string read EndTagText;
    end;

    TTagInformation = record
      Name: string;
      TagType: TTagType;
      ClosingTag: string;
      constructor Create(const AName: string; ATagType: TTagType; const AClosingTag: string);
    end;

    TRenderAttribute = record
      Name: string;
      Value: TdxNullableString;
      Key: TdxHtmlTextWriterAttribute;
      Encode: Boolean;
      IsUrl: Boolean;
      function ToString: string;
    end;

    TAttributeInformation = record
      Name: string;
      IsUrl: Boolean;
      Encode: Boolean;
      constructor Create(const AName: string; AEncode, AIsUrl: Boolean);
    end;

    TLayout = class
    strict private
      FAlign: TdxWebHorizontalAlign;
      FWrap: Boolean;
    public
      constructor Create(AAlignment: TdxWebHorizontalAlign; AWrapping: Boolean);

      property Align: TdxWebHorizontalAlign read FAlign write FAlign;
      property Wrap: Boolean read FWrap write FWrap;
    end;
{$ENDREGION}
  strict private
    class var
      FAttributeLookup: TdxNamedOrdinalDictionary<TdxHtmlTextWriterAttribute>;
      FTagsLookup: TdxNamedOrdinalDictionary<TdxHtmlTextWriterTag>;
      FAttributeNameLookup: array[TdxHtmlTextWriterAttribute] of TAttributeInformation;
      FTagNameLookup: array[TdxHtmlTextWriterTag] of TTagInformation;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FAttributesList: TList<TRenderAttribute>;
    FCurrentLayout: TLayout;
    FCurrentWrittenLayout: TLayout;
    FEndTags: TList<TTagStackEntry>;
    FInlineCount: Integer;
    FIsDescendant: Boolean;
    FStyleList: TList<TdxWebRenderStyle>;
    FTagIndex: TdxHtmlTextWriterTag;
    FTagKey: TdxHtmlTextWriterTag;
    FTagName: string;
    FIndentLevel: Integer;
    FTabsPending: Boolean;
    FTabString: string;
    FWriter: TTextWriter;
    procedure OpenDiv(ALayout: TLayout; AWriteHorizontalAlign: Boolean; AWriteWrapping: Boolean); overload;
    procedure SetIndent(const AValue: Integer);
    procedure SetTagKey(const AValue: TdxHtmlTextWriterTag);
    procedure SetTagName(const AValue: string);
  protected
    procedure AddAttribute(const AName: string; const AValue: TdxNullableString; AKey: TdxHtmlTextWriterAttribute; AEncode: Boolean = False; AIsUrl: Boolean = False); overload; virtual;
    procedure AddStyleAttribute(const AName: string; const AValue: string; AKey: TdxHtmlTextWriterStyle); overload; virtual;
    function EncodeAttributeValue(const AValue: TdxNullableString; AEncode: Boolean): TdxNullableString; overload;
    function EncodeAttributeValue(AAttrKey: TdxHtmlTextWriterAttribute; const AValue: TdxNullableString): TdxNullableString; overload; virtual;
    function EncodeUrl(const AUrl: string): string;
    procedure FilterAttributes; virtual;
    function GetAttributeKey(const AAttrName: string): TdxHtmlTextWriterAttribute;
    function GetAttributeName(AAttrKey: TdxHtmlTextWriterAttribute): string;
    function GetStyleKey(const AStyleName: string): TdxHtmlTextWriterStyle;
    function GetStyleName(AStyleKey: TdxHtmlTextWriterStyle): string;
    function GetTagKey(const ATagName: string): TdxHtmlTextWriterTag; virtual;
    function GetTagName(ATagKey: TdxHtmlTextWriterTag): string; virtual;
    function IsAttributeDefined(AKey: TdxHtmlTextWriterAttribute): Boolean; overload;
    function IsAttributeDefined(AKey: TdxHtmlTextWriterAttribute; out AValue: string): Boolean; overload;
    function IsStyleAttributeDefined(AKey: TdxHtmlTextWriterStyle): Boolean; overload;
    function IsStyleAttributeDefined(AKey: TdxHtmlTextWriterStyle; out AValue: string): Boolean; overload;
    function OnAttributeRender(const AName: string; const AValue: string; AKey: TdxHtmlTextWriterAttribute): Boolean; virtual;
    function OnStyleAttributeRender(const AName: string; const AValue: string; AKey: TdxHtmlTextWriterStyle): Boolean; virtual;
    function OnTagRender(const AName: string; AKey: TdxHtmlTextWriterTag): Boolean; virtual;
    procedure OpenDiv; overload; virtual;
    procedure OutputTabs; virtual;
    function PopEndTag: string;
    procedure PushEndTag(const AEndTag: string);
    class procedure RegisterAttribute(const AName: string; AKey: TdxHtmlTextWriterAttribute; AEncode: Boolean = False; AIsUrl: Boolean = False); static;
    class procedure RegisterStyle(const AName: string; AKey: TdxHtmlTextWriterStyle); static;
    class procedure RegisterTag(const AName: string; AKey: TdxHtmlTextWriterTag; AType: TTagType = TTagType.Other); static;
    function RenderAfterContent: string; virtual;
    function RenderAfterTag: string; virtual;
    function RenderBeforeContent: string; virtual;
    function RenderBeforeTag: string; virtual;
    procedure WriteUrlEncodedString(const AText: string; AArgument: Boolean);
    function GetRenderDivAroundHiddenInputs: Boolean; virtual;

    property TagKey: TdxHtmlTextWriterTag read FTagKey write SetTagKey;
    property TagName: string read FTagName write SetTagName;
  public
    constructor Create(AWriter: TTextWriter); overload;
    constructor Create(AWriter: TTextWriter; const ATabString: string); overload;
    destructor Destroy; override;
    procedure AddAttribute(const AName: string; const AValue: TdxNullableString); overload;
    procedure AddAttribute(const AName: string; const AValue: string); overload;
    procedure AddAttribute(AKey: TdxHtmlTextWriterAttribute; const AValue: TdxNullableString); overload;
    procedure AddAttribute(AKey: TdxHtmlTextWriterAttribute; const AValue: string); overload;
    procedure AddAttribute(const AName: string; const AValue: TdxNullableString; AEndode: Boolean); overload;
    procedure AddAttribute(AKey: TdxHtmlTextWriterAttribute; const AValue: TdxNullableString; AEncode: Boolean); overload;
    procedure AddStyleAttribute(const AName: string; const AValue: string); overload;
    procedure AddStyleAttribute(AKey: TdxHtmlTextWriterStyle; const AValue: string); overload;
    procedure BeginRender; virtual;
    procedure Close; override;
    procedure EndRender; virtual;
    procedure EnterStyle(AStyle: TdxWebStyle; ATag: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span); virtual;
    procedure ExitStyle(AStyle: TdxWebStyle; ATag: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span); virtual;
    procedure Flush; override;
    function IsValidFormAttribute(const AAttribute: string): Boolean; virtual;
    procedure RenderBeginTag(const ATagName: string); overload;
    procedure RenderBeginTag(ATagKey: TdxHtmlTextWriterTag); overload;
    procedure RenderEndTag; virtual;
    procedure Write(AValue: Boolean); override;
    procedure Write(AValue: Char); override;
    procedure Write(const ABuffer: TArray<Char>); override;
    procedure Write(AValue: Double); override;
    procedure Write(AValue: Integer); override;
    procedure Write(AValue: Int64); override;
    procedure Write(AValue: TObject); override;
    procedure Write(AValue: Single); override;
    procedure Write(const S: string); override;
    procedure Write(const ABuffer: TArray<Char>; AIndex, ACount: Integer); override;
    procedure WriteAttribute(const AName, AValue: string; AEncode: Boolean = False); virtual;
    procedure WriteBeginTag(const ATagName: string); virtual;
    procedure WriteBreak; virtual;
    procedure WriteEncodedText(const AText: string); virtual;
    procedure WriteEncodedUrl(const AUrl: string); virtual;
    procedure WriteEncodedUrlParameter(const AUrlText: string); virtual;
    procedure WriteEndTag(const ATagName: string); virtual;
    procedure WriteFullBeginTag(const ATagName: string); virtual;
    procedure WriteHtmlAttributeEncode(const S: string);
    procedure WriteLine; override;
    procedure WriteLine(AValue: Boolean); override;
    procedure WriteLine(AValue: Char); override;
    procedure WriteLine(AValue: Integer); override;
    procedure WriteLine(const ABuffer: TArray<Char>); override;
    procedure WriteLine(AValue: Double); override;
    procedure WriteLine(AValue: Int64); override;
    procedure WriteLine(AValue: TObject); override;
    procedure WriteLine(AValue: Single); override;
    procedure WriteLine(const S: string); override;
    procedure WriteLine(const ABuffer: TArray<Char>; AIndex, ACount: Integer); override;
    procedure WriteLineNoTabs(const S: string);
    procedure WriteObsoleteBreak;
    procedure WriteStyleAttribute(const AName, AValue: string; AEncode: Boolean = False); overload; virtual;

    property Indent: Integer read FIndentLevel write SetIndent;
    property InnerWriter: TTextWriter read FWriter write FWriter;
    property RenderDivAroundHiddenInputs: Boolean read GetRenderDivAroundHiddenInputs;
  end;

implementation

uses
  Contnrs, Character, StrUtils,

  dxStringHelper,
  dxRichEdit.Utils.Exceptions,
  dxEncoding,
  dxRichEdit.Utils.NumberParser;

{ TdxSimpleBitVector32 }

class function TdxSimpleBitVector32.Create: TdxSimpleBitVector32;
begin
  Result.FValue := 0;
end;

procedure TdxSimpleBitVector32.Clear(const Value: Cardinal);
begin
  FValue := FValue and not Value;
end;

function TdxSimpleBitVector32.GetItem(const Value: Cardinal): Boolean;
begin
  Result := FValue and Value = Value;
end;

procedure TdxSimpleBitVector32.&Set(const Value: Cardinal);
begin
  FValue := FValue or Value;
end;

{ TdxWebUnit }

constructor TdxWebUnit.Create(const AValue: string; const ACulture: TdxCultureInfo; ADefaultType: TdxWebUnitType);
var
  S, AText: string;
  ALength, ANum, I: Integer;
  C: Char;
begin
  if AValue = '' then
  begin
    FValue := 0.0;
    FType := TdxWebUnitType(0);
  end
  else
  begin
    S := LowerCase(Trim(AValue));
    ALength := Length(S);
    ANum := -1;
    for I := 0 to ALength - 1 do
    begin
      C := S[I + 1];
      if ((C < '0') or (C > '9')) and (C <> '-') and (C <> '.') and (C <> ',') then
        Break;
      ANum := I;
    end;
    if ANum = -1 then
      raise EParserError.Create('UnitParseNoDigits');

    if ANum < ALength - 1 then
      FType := GetTypeFromString(Trim(TdxStringHelper.Substring(S, ANum + 1)))
    else
      FType := ADefaultType;
    AText := TdxStringHelper.Substring(S, 0, ANum + 1);
    try
      FValue := StrToFloat(AText, ACulture.FormatSettings);
      if FType = TdxWebUnitType.Pixel then
        FValue := Trunc(FValue);
    except
      raise EParserError.Create('UnitParseNumericPart')
    end;
    if (FValue < MinValue) or (FValue > MaxValue) then
      raise EArgumentOutOfRangeException.Create('value');
  end;
end;

constructor TdxWebUnit.Create(const AValue: string; const ACulture: TdxCultureInfo);
begin
  Create(AValue, ACulture, TdxWebUnitType.Pixel);
end;

constructor TdxWebUnit.Create(const AValue: string);
begin
  Create(AValue, TdxCultureInfo.CurrentCulture, TdxWebUnitType.Pixel);
end;

constructor TdxWebUnit.Create(AValue: Double; AType: TdxWebUnitType);
begin
  if (AValue < MinValue) or (AValue > MaxValue) then
    raise EArgumentOutOfRangeException.Create('value');
  if AType = TdxWebUnitType.Pixel then
    FValue := Trunc(AValue)
  else
    FValue := AValue;
  FType := AType;
end;

constructor TdxWebUnit.Create(AValue: Double);
begin
  if (AValue < MinValue) or (AValue > MaxValue) then
    raise EArgumentOutOfRangeException.Create('value');
  FValue := Trunc(AValue);
  FType := TdxWebUnitType.Pixel;
end;

constructor TdxWebUnit.Create(AValue: Integer);
begin
  if (AValue < MinValue) or (AValue > MaxValue) then
    raise EArgumentOutOfRangeException.Create('value');
  FValue := AValue;
  FType := TdxWebUnitType.Pixel;
end;

{$IFDEF DELPHIXE3}
class constructor TdxWebUnit.Initialize;
begin
  FEmpty := TdxWebUnit.Create('');
end;
{$ELSE}
var
  EmptyWebUnit: TdxWebUnit;

class function TdxWebUnit.GetEmpty: TdxWebUnit;
begin
  Result := EmptyWebUnit;
end;

class constructor TdxWebUnit.Initialize;
begin
  EmptyWebUnit := TdxWebUnit.Create('');
end;
{$ENDIF}

class operator TdxWebUnit.Equal(const A, B: TdxWebUnit): Boolean;
begin
  Result := (A.FType = B.FType) and (A.FValue = B.FValue);
end;

class operator TdxWebUnit.NotEqual(const A, B: TdxWebUnit): Boolean;
begin
   Result := not (A = B);
end;

class function TdxWebUnit.GetStringFromType(AType: TdxWebUnitType): string;
begin
  case AType of
    TdxWebUnitType.Pixel:
      Exit('px');
    TdxWebUnitType.Point:
      Exit('pt');
    TdxWebUnitType.Pica:
      Exit('pc');
    TdxWebUnitType.Inch:
      Exit('in');
    TdxWebUnitType.Mm:
      Exit('mm');
    TdxWebUnitType.Cm:
      Exit('cm');
    TdxWebUnitType.Percentage:
      Exit('%');
    TdxWebUnitType.Em:
      Exit('em');
    TdxWebUnitType.Ex:
      Exit('ex');
  end;
  Result := '';
end;

class function TdxWebUnit.GetTypeFromString(const AValue: string): TdxWebUnitType;
begin
  if AValue = '' then
    Exit(TdxWebUnitType.Pixel);
  if AValue = 'px' then
    Exit(TdxWebUnitType.Pixel);
  if AValue = 'pt' then
    Exit(TdxWebUnitType.Point);
  if AValue = '%' then
    Exit(TdxWebUnitType.Percentage);
  if AValue = 'pc' then
    Exit(TdxWebUnitType.Pica);
  if AValue = 'in' then
    Exit(TdxWebUnitType.Inch);
  if AValue = 'mm' then
    Exit(TdxWebUnitType.Mm);
  if AValue = 'cm' then
    Exit(TdxWebUnitType.Cm);
  if AValue = 'em' then
    Exit(TdxWebUnitType.Em);
  if AValue <> 'ex' then
    raise EArgumentOutOfRangeException.Create('value');
  Result := TdxWebUnitType.Ex;
end;

class function TdxWebUnit.Parse(const S: string): TdxWebUnit;
begin
  Result := TdxWebUnit.Create(S, TdxCultureInfo.CurrentCulture);
end;

class function TdxWebUnit.Parse(const S: string; const ACulture: TdxCultureInfo): TdxWebUnit;
begin
  Result := TdxWebUnit.Create(S, ACulture);
end;

class function TdxWebUnit.Percentage(N: Double): TdxWebUnit;
begin
  Result := TdxWebUnit.Create(N, TdxWebUnitType.Percentage);
end;

class function TdxWebUnit.Pixel(N: Integer): TdxWebUnit;
begin
  Result := TdxWebUnit.Create(N);
end;

class function TdxWebUnit.Point(N: Integer): TdxWebUnit;
begin
  Result := TdxWebUnit.Create(Int(N), TdxWebUnitType.Point);
end;

function TdxWebUnit.GetIsEmpty: Boolean;
begin
  Result := FType = TdxWebUnitType(0);
end;

function TdxWebUnit.GetType: TdxWebUnitType;
begin
  if not IsEmpty then
    Exit(FType);
  Result := TdxWebUnitType.Pixel;
end;

function TdxWebUnit.ToString: string;
begin
  Result := ToString(TdxCultureInfo.CurrentCulture);
end;

function TdxWebUnit.ToString(const ACulture: TdxCultureInfo): string;
var
  S: string;
begin
  if IsEmpty then
    Exit('');

  if FType = TdxWebUnitType.Pixel then
    S := Format('%d', [Trunc(FValue)], ACulture.FormatSettings)
  else
    S := Format('%g', [Trunc(FValue)], ACulture.FormatSettings);
  Result := S + GetStringFromType(FType);
end;

{ TdxWebFontUnit }

constructor TdxWebFontUnit.Create(const AValue: string; const ACulture: TdxCultureInfo);
var
  C: Char;
  S: string;
begin
  FType := TdxWebFontSize.NotSet;
  FValue := TdxWebUnit.Empty;
  if AValue <> '' then
  begin
    C := {$IFDEF DELPHIXE4}AValue[1].ToLower{$ELSE}TCharacter.ToLower(AValue[1]){$ENDIF};
    case C of
      's':
        begin
          S := LowerCase(AValue);
          if S = 'small' then
          begin
            FType := TdxWebFontSize.Small;
            Exit;
          end;
          if S = 'smaller' then
          begin
            FType := TdxWebFontSize.Smaller;
            Exit;
          end;
        end;
      'l':
        begin
          S := LowerCase(AValue);
          if S = 'large' then
          begin
            FType := TdxWebFontSize.Large;
            Exit;
          end;
          if S = 'larger' then
          begin
            FType := TdxWebFontSize.Larger;
            Exit;
          end;
        end;
      'x':
        begin
          S := LowerCase(AValue);
          if (S = 'xx-small') or (S = 'xxsmall') then
          begin
            FType := TdxWebFontSize.XXSmall;
            Exit;
          end;
          if (S = 'x-small') or (S = 'xsmall') then
          begin
            FType := TdxWebFontSize.XSmall;
            Exit;
          end;
          if (S = 'x-large') or (S = 'xlarge') then
          begin
            FType := TdxWebFontSize.XLarge;
            Exit;
          end;
          if (S = 'xx-large') or (S = 'xxlarge') then
          begin
            FType := TdxWebFontSize.XXLarge;
            Exit;
          end;
        end;
      else
        if (C = 'm') and (LowerCase(AValue) = 'medium') then
        begin
          FType := TdxWebFontSize.Medium;
          Exit;
        end;
    end;
    FValue := TdxWebUnit.Create(AValue, ACulture, TdxWebUnitType.Point);
    FType := TdxWebFontSize.AsUnit;
  end;
end;

constructor TdxWebFontUnit.Create(const AValue: string);
begin
  Create(AValue, TdxCultureInfo.CurrentCulture);
end;

constructor TdxWebFontUnit.Create(AValue: Double; AType: TdxWebUnitType);
begin
  Create(TdxWebUnit.Create(AValue, AType));
end;

constructor TdxWebFontUnit.Create(AValue: Double);
begin
  Create(TdxWebUnit.Create(AValue, TdxWebUnitType.Point));
end;

constructor TdxWebFontUnit.Create(AValue: Integer);
begin
  FType := TdxWebFontSize.AsUnit;
  FValue := TdxWebUnit.Point(AValue);
end;

constructor TdxWebFontUnit.Create(AValue: TdxWebUnit);
begin
  FType := TdxWebFontSize.NotSet;
  if not AValue.IsEmpty then
  begin
    FType := TdxWebFontSize.AsUnit;
    FValue := AValue;
  end
  else
    FValue := TdxWebUnit.Empty;
end;

constructor TdxWebFontUnit.Create(AType: TdxWebFontSize);
begin
  FType := AType;
  if FType = TdxWebFontSize.AsUnit then
    FValue := TdxWebUnit.Point(10)
  else
    FValue := TdxWebUnit.Empty;
end;

{$IFDEF DELPHIXE3}
class constructor TdxWebFontUnit.Initialize;
begin
  FEmpty.FType := TdxWebFontSize.NotSet;
  FEmpty.FValue := TdxWebUnit.Empty;

  FSmaller := TdxWebFontUnit.Create(TdxWebFontSize.Smaller);
  FLarger := TdxWebFontUnit.Create(TdxWebFontSize.Larger);
  FXXSmall := TdxWebFontUnit.Create(TdxWebFontSize.XXSmall);
  FXSmall := TdxWebFontUnit.Create(TdxWebFontSize.XSmall);
  FSmall := TdxWebFontUnit.Create(TdxWebFontSize.Small);
  FMedium := TdxWebFontUnit.Create(TdxWebFontSize.Medium);
  FLarge := TdxWebFontUnit.Create(TdxWebFontSize.Large);
  FXLarge := TdxWebFontUnit.Create(TdxWebFontSize.XLarge);
  FXXLarge := TdxWebFontUnit.Create(TdxWebFontSize.XXLarge);
end;
{$ELSE}

var
  EmptyWebFontUnit: TdxWebFontUnit;
  SmallerWebFontUnit: TdxWebFontUnit;
  LargerWebFontUnit: TdxWebFontUnit;
  XXSmallWebFontUnit: TdxWebFontUnit;
  XSmallWebFontUnit: TdxWebFontUnit;
  SmallWebFontUnit: TdxWebFontUnit;
  MediumWebFontUnit: TdxWebFontUnit;
  LargeWebFontUnit: TdxWebFontUnit;
  XLargeWebFontUnit: TdxWebFontUnit;
  XXLargeWebFontUnit: TdxWebFontUnit;

class constructor TdxWebFontUnit.Initialize;
begin
  EmptyWebFontUnit.FType := TdxWebFontSize.NotSet;
  EmptyWebFontUnit.FValue := TdxWebUnit.Empty;

  SmallerWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.Smaller);
  LargerWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.Larger);
  XXSmallWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.XXSmall);
  XSmallWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.XSmall);
  SmallWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.Small);
  MediumWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.Medium);
  LargeWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.Large);
  XLargeWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.XLarge);
  XXLargeWebFontUnit := TdxWebFontUnit.Create(TdxWebFontSize.XXLarge);
end;

class function TdxWebFontUnit.GetEmpty: TdxWebFontUnit;
begin
  Result := EmptyWebFontUnit;
end;

class function TdxWebFontUnit.GetLarge: TdxWebFontUnit;
begin
  Result := LargeWebFontUnit;
end;

class function TdxWebFontUnit.GetLarger: TdxWebFontUnit;
begin
  Result := LargerWebFontUnit;
end;

class function TdxWebFontUnit.GetMedium: TdxWebFontUnit;
begin
  Result := MediumWebFontUnit;
end;

class function TdxWebFontUnit.GetSmall: TdxWebFontUnit;
begin
  Result := SmallWebFontUnit;
end;

class function TdxWebFontUnit.GetSmaller: TdxWebFontUnit;
begin
  Result := SmallerWebFontUnit;
end;

class function TdxWebFontUnit.GetXLarge: TdxWebFontUnit;
begin
  Result := XLargeWebFontUnit;
end;

class function TdxWebFontUnit.GetXSmall: TdxWebFontUnit;
begin
  Result := XSmallWebFontUnit;
end;

class function TdxWebFontUnit.GetXXLarge: TdxWebFontUnit;
begin
  Result := XXLargeWebFontUnit;
end;

class function TdxWebFontUnit.GetXXSmall: TdxWebFontUnit;
begin
  Result := XXSmallWebFontUnit;
end;
{$ENDIF}

class operator TdxWebFontUnit.Equal(const A, B: TdxWebFontUnit): Boolean;
begin
  Result := (A.FType = B.FType) and (A.FValue = B.FValue);
end;

class operator TdxWebFontUnit.NotEqual(const A, B: TdxWebFontUnit): Boolean;
begin
  Result := not (A = B);
end;

function TdxWebFontUnit.GetIsEmpty: Boolean;
begin
  Result := FType = TdxWebFontSize.NotSet;
end;

class function TdxWebFontUnit.Parse(const S: string): TdxWebFontUnit;
begin
  Result := TdxWebFontUnit.Create(S, TdxCultureInfo.InvariantCulture);
end;

class function TdxWebFontUnit.Parse(const S: string; const ACulture: TdxCultureInfo): TdxWebFontUnit;
begin
  Result := TdxWebFontUnit.Create(S, ACulture);
end;

class function TdxWebFontUnit.Point(N: Integer): TdxWebFontUnit;
begin
  Result := TdxWebFontUnit.Create(N);
end;

function TdxWebFontUnit.ToString: string;
begin
  Result := ToString(TdxCultureInfo.CurrentCulture);
end;

function TdxWebFontUnit.ToString(const ACulture: TdxCultureInfo): string;
const
  Sizes: array[TdxWebFontSize] of string = (
    '', // NotSet,
    '', // AsUnit,
    'smaller',
    'larger',
    'xx-small',
    'x-small',
    'small',
    'medium',
    'large',
    'x-large',
    'xx-large');
begin
  if FType = TdxWebFontSize.AsUnit then
    Result := FValue.ToString(ACulture)
  else
    Result := Sizes[FType];
end;

{ TdxWebFontInfo }

constructor TdxWebFontInfo.Create(AOwner: TdxWebStyle);
begin
  FOwner := AOwner;
end;

procedure TdxWebFontInfo.ClearDefaults;
begin
  if Length(Names) = 0 then
  begin
    FOwner.ViewState.Remove('Font_Names');
    FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontNames);
  end;
  if Size = TdxWebFontUnit.Empty then
  begin
    FOwner.ViewState.Remove('Font_Size');
    FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontSize);
  end;
  if not Bold then
    ResetBold;
  if not Italic then
    ResetItalic;
  if not Underline then
    ResetUnderline;
  if not Overline then
    ResetOverline;
  if not Strikeout then
    ResetStrikeout;
end;

procedure TdxWebFontInfo.CopyFrom(const F: TdxWebFontInfo);
var
  AOwner: TdxWebStyle;
begin
  if F <> nil then
  begin
    AOwner := F.Owner;
    if Length(AOwner.RegisteredCssClass) <> 0 then
    begin
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontNames) then
        ResetNames;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontSize) and (F.Size <> TdxWebFontUnit.Empty) then
        ResetFontSize;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontBold) then
        ResetBold;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontItalic) then
        ResetItalic;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontOverline) then
        ResetOverline;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontStrikeout) then
        ResetStrikeout;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontUnderline) then
        ResetUnderline;
    end
    else
    begin
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontNames) then
        Names := F.Names;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontSize) and (F.Size <> TdxWebFontUnit.Empty) then
        Size := F.Size;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontBold) then
        Bold := F.Bold;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontItalic) then
        Italic := F.Italic;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontOverline) then
        Overline := F.Overline;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontStrikeout) then
        Strikeout := F.Strikeout;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontUnderline) then
        Underline := F.Underline;
    end;
  end;
end;

procedure TdxWebFontInfo.MergeWith(const F: TdxWebFontInfo);
var
  AOwner: TdxWebStyle;
begin
  if F <> nil then
  begin
    AOwner := F.Owner;
    if Length(AOwner.RegisteredCssClass) = 0 then
    begin
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontNames) and not FOwner.IsSet(TdxWebStyle.TAssignedValue.FontNames) then
        Names := F.Names;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontSize) and (not FOwner.IsSet(TdxWebStyle.TAssignedValue.FontSize) or (Size = TdxWebFontUnit.Empty)) then
        Size := F.Size;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontBold) and not FOwner.IsSet(TdxWebStyle.TAssignedValue.FontBold) then
        Bold := F.Bold;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontItalic) and not FOwner.IsSet(TdxWebStyle.TAssignedValue.FontItalic) then
        Italic := F.Italic;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontOverline) and not FOwner.IsSet(TdxWebStyle.TAssignedValue.FontOverline) then
        Overline := F.Overline;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontStrikeout) and not FOwner.IsSet(TdxWebStyle.TAssignedValue.FontStrikeout) then
        Strikeout := F.Strikeout;
      if AOwner.IsSet(TdxWebStyle.TAssignedValue.FontUnderline) and not FOwner.IsSet(TdxWebStyle.TAssignedValue.FontUnderline) then
        Underline := F.Underline;
    end;
  end;
end;

procedure TdxWebFontInfo.Reset;
begin
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontNames) then
    ResetNames;
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontSize) then
    ResetFontSize;
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontBold) then
    ResetBold;
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontItalic) then
    ResetItalic;
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontUnderline) then
    ResetUnderline;
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontOverline) then
    ResetOverline;
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontStrikeout) then
    ResetStrikeout;
end;

procedure TdxWebFontInfo.ResetBold;
begin
  FOwner.ViewState.Remove('Font_Bold');
  FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontBold);
end;

procedure TdxWebFontInfo.ResetFontSize;
begin
  FOwner.ViewState.Remove('Font_Size');
  FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontSize);
end;

procedure TdxWebFontInfo.ResetItalic;
begin
  FOwner.ViewState.Remove('Font_Italic');
  FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontItalic);
end;

procedure TdxWebFontInfo.ResetNames;
begin
  FOwner.ViewState.Remove('Font_Names');
  FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontNames);
end;

procedure TdxWebFontInfo.ResetOverline;
begin
  FOwner.ViewState.Remove('Font_Overline');
  FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontOverline);
end;

procedure TdxWebFontInfo.ResetStrikeout;
begin
  FOwner.ViewState.Remove('Font_Strikeout');
  FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontStrikeout);
end;

procedure TdxWebFontInfo.ResetUnderline;
begin
  FOwner.ViewState.Remove('Font_Underline');
  FOwner.ClearBit(TdxWebStyle.TAssignedValue.FontUnderline);
end;

function TdxWebFontInfo.ToString: string;
var
  AName: string;
begin
  Result := Size.ToString(TdxCultureInfo.InvariantCulture);
  AName := Name;
  if Result = '' then
    Exit(AName);

  if AName <> '' then
    Result := Format('%s, %s', [AName, Result]);
end;

function TdxWebFontInfo.GetBold: Boolean;
begin
  Result := FOwner.IsSet(TdxWebStyle.TAssignedValue.FontBold) and FOwner.ViewState['Font_Bold'].AsBoolean;
end;

procedure TdxWebFontInfo.SetBold(const AValue: Boolean);
begin
  FOwner.ViewState['Font_Bold'] := AValue;
  FOwner.SetBit(TdxWebStyle.TAssignedValue.FontBold);
end;

function TdxWebFontInfo.GetItalic: Boolean;
begin
  Result := FOwner.IsSet(TdxWebStyle.TAssignedValue.FontItalic) and FOwner.ViewState['Font_Italic'].AsBoolean;
end;

procedure TdxWebFontInfo.SetItalic(const AValue: Boolean);
begin
  FOwner.ViewState['Font_Italic'] := AValue;
  FOwner.SetBit(TdxWebStyle.TAssignedValue.FontItalic);
end;

function TdxWebFontInfo.GetName: string;
var
  ANames: TArray<string>;
begin
  ANames := Names;
  if Length(ANames) > 0 then
    Result := ANames[0]
  else
    Result := '';
end;

procedure TdxWebFontInfo.SetName(const AValue: string);
begin
  if AValue = '' then
    Names := nil
  else
    Names := TArray<string>.Create(AValue);
end;

function TdxWebFontInfo.GetNames: TArray<string>;
begin
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontNames) then
    Result := FOwner.ViewState['Font_Names'].AsType<TArray<string>>
  else
    Result := nil;
end;

procedure TdxWebFontInfo.SetNames(const AValue: TArray<string>);
begin
  FOwner.ViewState['Font_Names'] := TValue.From<TArray<string>>(AValue);
  FOwner.SetBit(TdxWebStyle.TAssignedValue.FontNames);
end;

function TdxWebFontInfo.GetOverline: Boolean;
begin
  Result := FOwner.IsSet(TdxWebStyle.TAssignedValue.FontOverline) and FOwner.ViewState['Font_Overline'].AsBoolean;
end;

procedure TdxWebFontInfo.SetOverline(const AValue: Boolean);
begin
  FOwner.ViewState['Font_Overline'] := AValue;
  FOwner.SetBit(TdxWebStyle.TAssignedValue.FontOverline);
end;

function TdxWebFontInfo.GetSize: TdxWebFontUnit;
begin
  if FOwner.IsSet(TdxWebStyle.TAssignedValue.FontSize) then
    Result := FOwner.ViewState['Font_Size'].AsType<TdxWebFontUnit>
  else
    Result := TdxWebFontUnit.Empty;
end;

procedure TdxWebFontInfo.SetSize(const AValue: TdxWebFontUnit);
begin
  if (AValue.&Type = TdxWebFontSize.AsUnit) and (AValue.&Unit.Value < 0.0) then
    raise EArgumentOutOfRangeException.Create('value');
  FOwner.ViewState['Font_Size'] := TValue.From<TdxWebFontUnit>(AValue);
  FOwner.SetBit(TdxWebStyle.TAssignedValue.FontSize);
end;

function TdxWebFontInfo.GetStrikeout: Boolean;
begin
  Result := FOwner.IsSet(TdxWebStyle.TAssignedValue.FontStrikeout) and FOwner.ViewState['Font_Strikeout'].AsBoolean;
end;

procedure TdxWebFontInfo.SetStrikeout(const AValue: Boolean);
begin
  FOwner.ViewState['Font_Strikeout'] := AValue;
  FOwner.SetBit(TdxWebStyle.TAssignedValue.FontStrikeout);
end;

function TdxWebFontInfo.GetUnderline: Boolean;
begin
  Result := FOwner.IsSet(TdxWebStyle.TAssignedValue.FontUnderline) and FOwner.ViewState['Font_Underline'].AsBoolean;
end;

procedure TdxWebFontInfo.SetUnderline(const AValue: Boolean);
begin
  FOwner.ViewState['Font_Underline'] := AValue;
  FOwner.SetBit(TdxWebStyle.TAssignedValue.FontUnderline);
end;

{ TdxWebControlBase }

constructor TdxWebControlBase.Create;
begin
  inherited Create;
  FFlags := TdxSimpleBitVector32.Create;
end;

destructor TdxWebControlBase.Destroy;
begin
  FreeAndNil(FViewState);
  FreeAndNil(FControls);
  FreeAndNil(FNamedControls);
  inherited Destroy;
end;

procedure TdxWebControlBase.SetChildControlsCreated(const AValue: Boolean);
begin
  if not AValue and FFlags[8] then
    Controls.Clear;
  if AValue then
    FFlags.&Set(8)
  else
    FFlags.Clear(8);
end;

function TdxWebControlBase.GetChildControlsCreated: Boolean;
begin
  Result := FFlags[8];
end;

function TdxWebControlBase.GetClientID: string;
begin
  EnsureID;
  Result := UniqueID;
  if (Result <> '') and (TdxStringHelper.IndexOf(Result, IdSeparator) >= 0) then
    Exit(TdxStringHelper.Replace(Result, IdSeparator, '_'));
end;

function TdxWebControlBase.GetControls: TdxWebControlCollection;
begin
  if FControls = nil then
    FControls := CreateControlCollection;
  Result := FControls;
end;

function TdxWebControlBase.GetEnableTheming: Boolean;
begin
  if not FFlags[$2000] and (Parent <> nil) then
    Exit(Parent.EnableTheming);
  Result := not FFlags[$1000];
end;

procedure TdxWebControlBase.SetEnableTheming(const AValue: Boolean);
begin
  if FControlState >= TdxWebControlState.FrameworkInitialized then
    TdxRichEditExceptions.ThrowInvalidOperationException('PropertySetBeforePreInitOrAddToControls');
  if not AValue then
    FFlags.&Set($1000)
  else
    FFlags.Clear($1000);
  FFlags.&Set($2000);
end;

function TdxWebControlBase.GetID: string;
begin
  if not FFlags[1] and not FFlags[$800] then
    Exit('');
  Result := FId;
end;

procedure TdxWebControlBase.SetID(const AValue: string);
var
  AStr: string;
begin
  AStr := FId;
  FId := AValue;
  ClearCachedUniqueIDRecursive;
  FFlags.&Set(1);
  FFlags.Clear($200000);
  if (FNamingContainer <> nil) and (AStr <> '') then
    FNamingContainer.DirtyNameTable;
end;

function TdxWebControlBase.GetIdSeparator: Char;
begin
  Result := ':';
end;

function TdxWebControlBase.GetIsChildControlStateCleared: Boolean;
begin
  Result := FFlags[$40000];
end;

function TdxWebControlBase.GetIsTrackingViewState: Boolean;
begin
  Result := FFlags[2];
end;

function TdxWebControlBase.GetNamingContainer: TdxWebControlBase;
begin
  if (FNamingContainer = nil) and (Parent <> nil) then
  begin
    if Parent.Flags[$80] then
      FNamingContainer := Parent
    else
      FNamingContainer := Parent.NamingContainer;
  end;
  Result := FNamingContainer;
end;

function TdxWebControlBase.GetParent: TdxWebControlBase;
begin
  Result := FParent;
end;

function TdxWebControlBase.GetUniqueID: string;
var
  ANamingContainer: TdxWebControlBase;
  AUniqueIDPrefix: string;
begin
  if FCachedUniqueID = '' then
  begin
    ANamingContainer := NamingContainer;
    if ANamingContainer = nil then
      Exit(FId);
    if FId = '' then
      GenerateAutomaticID;
    AUniqueIDPrefix := ANamingContainer.GetUniqueIDPrefix;
    if Length(AUniqueIDPrefix) = 0 then
      Exit(FId);
    FCachedUniqueID := AUniqueIDPrefix + FId;
  end;
  Result := FCachedUniqueID;
end;

function TdxWebControlBase.GetViewState: TdxStateBag;
begin
  if FViewState = nil then
    FViewState := TdxStateBag.Create;
  Result := FViewState;
end;

function TdxWebControlBase.GetVisible: Boolean;
begin
  if FFlags[$10] then
    Exit(False);
  if FParent <> nil then
    Exit(FParent.Visible);
  Result := True;
end;

function TdxWebControlBase.IsLiteral: Boolean;
begin
  Result := False;
end;

procedure TdxWebControlBase.SetVisible(const AValue: Boolean);
var
  AFlag: Boolean;
begin
  if FFlags[2] then
  begin
    AFlag := not FFlags[$10];
    if AFlag <> AValue then
      FFlags.&Set($20);
  end;
  if not AValue then
    FFlags.&Set($10)
  else
    FFlags.Clear($10);
end;

function TdxWebControlBase.ResolveClientUrl(const ARelativeUrl: string): string;
begin
  Result := ARelativeUrl;
end;

function TdxWebControlBase.FindControl(const AId: string): TdxWebControlBase;
begin
  Result := FindControl(AId, 0);
end;

procedure TdxWebControlBase.RenderControl(AWriter: TdxHtmlTextWriter);
begin
  if not FFlags[$10] and not FFlags[$200] then
    RenderControlInternal(AWriter);
end;

procedure TdxWebControlBase.RemovedControl(AControl: TdxWebControlBase);
begin
  if (FNamingContainer <> nil) and (AControl.id <> '') then
    FNamingContainer.DirtyNameTable;
  AControl.UnloadRecursive(False);
  AControl.FParent := nil;
  AControl.FNamingContainer := nil;
  AControl.FFlags.Clear($800);
  AControl.ClearCachedUniqueIDRecursive;
end;

function TdxWebControlBase.HasControls: Boolean;
begin
  Result := (FControls <> nil) and  (FControls.Count > 0);
end;

procedure TdxWebControlBase.ClearNamingContainer;
begin
  FNamedControlsID := 0;
  DirtyNameTable;
end;

procedure TdxWebControlBase.UnloadRecursive(ADispose: Boolean);
var
  ACount, I: Integer;
begin
  if FFlags[$200000] then
  begin
    FId := '';
    FFlags.Clear($200000);
  end;
  if FControls <> nil then
  begin
    ACount := Controls.Count;
    for I := 0 to ACount - 1 do
      Controls[I].UnloadRecursive(ADispose);
  end;
  OnUnload(TdxEventArgs.Empty);
  if ADispose then
    FreeAndNil(FControls);
end;

procedure TdxWebControlBase.DirtyNameTable;
begin
  FNamedControls := nil;
end;

function TdxWebControlBase.GetUniqueIDPrefix: string;
var
  AUniqueID: string;
begin
  if FUniqueIDPrefix = '' then
  begin
    AUniqueID := UniqueID;
    if AUniqueID <> '' then
      FUniqueIDPrefix := AUniqueID + IdSeparator
    else
      FUniqueIDPrefix := '';
  end;
  Result := FUniqueIDPrefix;
end;

procedure TdxWebControlBase.InitRecursive(ANamingContainer: TdxWebControlBase);
var
  ACount, I: Integer;
  AControl: TdxWebControlBase;
begin
  if FControls <> nil then
  begin
    if FFlags[$80] then
      ANamingContainer := Self;
    ACount := FControls.Count;
    for I := 0 to ACount - 1 do
    begin
      AControl := FControls[I];
      AControl.UpdateNamingContainer(ANamingContainer);
      if (AControl.ID = '') and (ANamingContainer <> nil) and not AControl.Flags[$40] then
        AControl.GenerateAutomaticID;
      AControl.InitRecursive(ANamingContainer);
    end;
  end;
  if FControlState < TdxWebControlState.Initialized then
  begin
    FControlState := TdxWebControlState.ChildrenInitialized;
    OnInit(TdxEventArgs.Empty);
    FControlState := TdxWebControlState.Initialized;
  end;
end;

function TdxWebControlBase.IsDescendentOf(AAncestor: TdxWebControlBase): Boolean;
var
  AParent: TdxWebControlBase;
begin
  AParent := Self;
  while (AParent <> AAncestor) and (AParent.Parent <> nil) do
    AParent := AParent.Parent;
  Result := AParent = AAncestor;
end;

procedure TdxWebControlBase.LoadRecursive;
var
  ACount, I: Integer;
begin
  if FControlState < TdxWebControlState.Loaded then
    OnLoad(TdxEventArgs.Empty);
  if FControls <> nil then
  begin
    ACount := Controls.Count;
    for I := 0 to ACount - 1 do
      Controls[I].LoadRecursive;
  end;
  if FControlState < TdxWebControlState.Loaded then
    FControlState := TdxWebControlState.Loaded;
end;

procedure TdxWebControlBase.PreRenderRecursiveInternal;
var
  ACount, I: Integer;
begin
  if not Visible then
    FFlags.&Set($10)
  else
  begin
    FFlags.Clear($10);
    EnsureChildControls;
    OnPreRender(TdxEventArgs.Empty);
    if FControls <> nil then
    begin
      ACount := Controls.Count;
      for I := 0 to ACount - 1 do
        Controls[I].PreRenderRecursiveInternal;
    end;
  end;
  FControlState := TdxWebControlState.PreRendered;
end;

procedure TdxWebControlBase.PreventAutoID;
begin
  if not FFlags[$80] then
    FFlags.&Set($40);
end;

procedure TdxWebControlBase.RenderChildrenInternal(AWriter: TdxHtmlTextWriter; const AChildren: TdxWebControlCollection);
var
  I: Integer;
begin
  if AChildren <> nil then
    for I := 0 to AChildren.Count - 1 do
      AChildren[I].RenderControl(AWriter);
end;

procedure TdxWebControlBase.AddedControl(AControl: TdxWebControlBase; AIndex: Integer);
var
  ANamingContainerCurrent: TdxWebControlBase;
begin
  if AControl.FParent <> nil then
    AControl.FParent.Controls.Remove(AControl);
  AControl.FParent := Self;
  AControl.FFlags.Clear($20000);
  if FFlags[$80] then
    ANamingContainerCurrent := Self
  else
    ANamingContainerCurrent := FNamingContainer;
  if ANamingContainerCurrent <> nil then
  begin
    AControl.UpdateNamingContainer(ANamingContainerCurrent);
    if (AControl.FId = '') and not AControl.FFlags[$40] then
      AControl.GenerateAutomaticID
    else
      if (AControl.id <> '') or (AControl.FControls <> nil) then
        ANamingContainerCurrent.DirtyNameTable;
  end;
  if FControlState >= TdxWebControlState.ChildrenInitialized then
  begin
    AControl.InitRecursive(ANamingContainerCurrent);
    if FControlState >= TdxWebControlState.ViewStateLoaded then
    begin
      if FControlState >= TdxWebControlState.Loaded then
      begin
        AControl.LoadRecursive;
        if FControlState >= TdxWebControlState.PreRendered then
          AControl.PreRenderRecursiveInternal;
      end;
    end;
  end;
end;

procedure TdxWebControlBase.OnInit(E: TdxEventArgs);
begin
end;

procedure TdxWebControlBase.OnLoad(E: TdxEventArgs);
begin
end;

procedure TdxWebControlBase.OnPreRender(E: TdxEventArgs);
begin
end;

procedure TdxWebControlBase.OnUnload(E: TdxEventArgs);
begin
end;

procedure TdxWebControlBase.BuildProfileTree(ACalcViewState: Boolean);
var
  AControl: TdxWebControlBase;
  I: Integer;
begin
  if FControls <> nil then
    for I := 0 to Controls.Count - 1 do
    begin
      AControl := Controls[I];
      AControl.BuildProfileTree(ACalcViewState);
    end;
end;

procedure TdxWebControlBase.CreateChildControls;
begin
end;

function TdxWebControlBase.CreateControlCollection: TdxWebControlCollection;
begin
  Result := TdxWebControlCollection.Create(Self);
end;

procedure TdxWebControlBase.EnsureChildControls;
begin
  if not ChildControlsCreated and not FFlags[$100] then
  begin
    FFlags.&Set($100);
    try
      ChildControlsCreated := True;
    finally
      FFlags.Clear($100);
    end;
  end;
end;

procedure TdxWebControlBase.EnsureID;
begin
  if FNamingContainer <> nil then
  begin
    if FId = '' then
      GenerateAutomaticID;
    FFlags.&Set($800);
  end;
end;

function TdxWebControlBase.FindControl(const AId: string; APathOffset: Integer): TdxWebControlBase;
var
  AStr: string;
  ANamingContainer, AControl2: TdxWebControlBase;
  ANum: Integer;
begin
  EnsureChildControls;
  if not FFlags[$80] then
  begin
    ANamingContainer := NamingContainer;
    if ANamingContainer <> nil then
      Exit(ANamingContainer.FindControl(AId, APathOffset));
    Exit(nil);
  end;
  if HasControls and (FNamedControls = nil) then
    EnsureNamedControlsTable;
  if FNamedControls = nil then
    Exit(nil);
  ANum := TdxStringHelper.IndexOfAny(AId, ['$', ':'], APathOffset);
  if ANum = -1 then
  begin
    AStr := TdxStringHelper.Substring(AId, APathOffset);
    Exit(Safe<TdxWebControlBase>.Cast(FNamedControls[AStr]));
  end;
  AStr := TdxStringHelper.Substring(AId, APathOffset, ANum - APathOffset);
  AControl2 := Safe<TdxWebControlBase>.Cast(FNamedControls[AStr]);
  if AControl2 = nil then
    Exit(nil);
  Result := AControl2.FindControl(AId, ANum + 1);
end;

function TdxWebControlBase.IsLiteralContent: Boolean;
begin
  Result := (FControls <> nil) and (FControls.Count = 1) and Controls[0].IsLiteral;
end;

procedure TdxWebControlBase.LoadControlState(ASavedState: TObject);
begin
end;

function TdxWebControlBase.OnBubbleEvent(ASource: TObject; AArgs: TdxEventArgs): Boolean;
begin
  Result := False;
end;

procedure TdxWebControlBase.OnDataBinding(E: TdxEventArgs);
begin
end;

procedure TdxWebControlBase.RaiseBubbleEvent(ASource: TObject; AArgs: TdxEventArgs);
var
  AControl: TdxWebControlBase;
begin
  AControl := Parent;
  while AControl <> nil do
  begin
    if AControl.OnBubbleEvent(ASource, AArgs) then
      Exit;
    AControl := AControl.Parent;
  end;
end;

procedure TdxWebControlBase.Render(AWriter: TdxHtmlTextWriter);
begin
  RenderChildren(AWriter);
end;

procedure TdxWebControlBase.RenderChildren(AWriter: TdxHtmlTextWriter);
begin
  RenderChildrenInternal(AWriter, FControls);
end;

procedure TdxWebControlBase.UpdateNamingContainer(ANamingContainer: TdxWebControlBase);
begin
  if (FNamingContainer <> nil) and (FNamingContainer <> ANamingContainer) then
    ClearCachedUniqueIDRecursive;
  FNamingContainer := ANamingContainer;
end;

procedure TdxWebControlBase.ClearCachedUniqueIDRecursive;
var
  ACount, I: Integer;
begin
  FCachedUniqueID := '';
  FUniqueIDPrefix := '';
  if FControls <> nil then
  begin
    ACount := FControls.Count;
    for I := 0 to ACount - 1 do
      Controls[I].ClearCachedUniqueIDRecursive;
  end;
end;

procedure TdxWebControlBase.EnsureNamedControlsTable;
begin
  FNamedControls := TdxNamedOrdinalDictionary<TdxWebControlBase>.Create;
  FillNamedControlsTable(Self, Controls);
end;

procedure TdxWebControlBase.FillNamedControlsTable(ANamingContainer: TdxWebControlBase; AControls: TdxWebControlCollection);
var
  AControl: TdxWebControlBase;
  I: Integer;
begin
  for I := 0 to AControls.Count - 1 do
  begin
    AControl := AControls[I];
    if AControl.ID <> '' then
    begin
      try
        ANamingContainer.FNamedControls.Add(AControl.ID, AControl);
      except
        raise Exception.Create('Duplicate id used')
      end;
    end;
    if AControl.HasControls and not AControl.Flags[$80] then
      FillNamedControlsTable(ANamingContainer, AControl.Controls);
  end;
end;

procedure TdxWebControlBase.GenerateAutomaticID;
var
  AIndex: Integer;
begin
  FFlags.&Set($200000);
  AIndex := FNamingContainer.FNamedControlsID;
  Inc(FNamingContainer.FNamedControlsID);
  FId := '_ctl' + IntToStr(AIndex);
  FNamingContainer.DirtyNameTable;
end;

procedure TdxWebControlBase.RenderControlInternal(AWriter: TdxHtmlTextWriter);
begin
  Render(AWriter);
end;

{ TdxWebControlCollection }

constructor TdxWebControlCollection.Create(AOwner: TdxWebControlBase);
begin
  inherited Create;
  FOwner := AOwner;
  FControls := TdxList<TdxWebControlBase>.Create;
end;

destructor TdxWebControlCollection.Destroy;
begin
  FreeAndNil(FControls);
  inherited Destroy;
end;

function TdxWebControlCollection.GetCount: Integer;
begin
  Result := FControls.Count;
end;

procedure TdxWebControlCollection.Add(AChild: TdxWebControlBase);
begin
  FControls.Add(AChild);
  FOwner.AddedControl(AChild, FControls.Count - 1);
end;

procedure TdxWebControlCollection.AddAt(AIndex: Integer; AChild: TdxWebControlBase);
begin
  if AIndex = -1 then
  begin
    Add(AChild);
    Exit;
  end;
  FControls.Insert(AIndex, AChild);
  FOwner.AddedControl(AChild, AIndex);
end;

procedure TdxWebControlCollection.Clear;
var
  I: Integer;
begin
  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
      RemoveAt(I);
end;

function TdxWebControlCollection.Contains(C: TdxWebControlBase): Boolean;
begin
  if C <> nil then
    Exit(FControls.Contains(C));
  Result := False;
end;

function TdxWebControlCollection.GetItem(Index: Integer): TdxWebControlBase;
begin
  Result := FControls[Index];
end;

function TdxWebControlCollection.IndexOf(AValue: TdxWebControlBase): Integer;
begin
  Result := FControls.IndexOf(AValue);
end;

procedure TdxWebControlCollection.Remove(AValue: TdxWebControlBase);
begin
  FControls.Remove(AValue);
end;

procedure TdxWebControlCollection.RemoveAt(AIndex: Integer);
var
  AControl: TdxWebControlBase;
begin
  AControl := Self[AIndex];
  FControls.Extract(AControl);
  try
    FOwner.RemovedControl(AControl);
  finally
    AControl.Free;
  end;
end;

{ TdxWebEmptyControlCollection }

procedure TdxWebEmptyControlCollection.Add(AChild: TdxWebControlBase);
begin
  raise ENotSupportedException.Create('Add');
end;

procedure TdxWebEmptyControlCollection.AddAt(AIndex: Integer; AChild: TdxWebControlBase);
begin
  raise ENotSupportedException.Create('AddAt');
end;

{ TdxCssStyleCollection }

constructor TdxCssStyleCollection.Create(AState: TdxStateBag = nil);
begin
  inherited Create;
  FState := AState;
end;

destructor TdxCssStyleCollection.Destroy;
begin
  FreeAndNil(FHtmlStyleTable);
  FreeAndNil(FStyleTable);
  inherited Destroy;
end;

class constructor TdxCssStyleCollection.Initialize;
begin
  FStyleAttribRegex := TRegex.Create('\G(\s*(;\s*)*(?<stylename>[^:]+?)\s*:\s*(?<styleval>[^;]*))*\s*(;\s*)*$',
    [TRegExOption.roExplicitCapture, TRegExOption.roMultiLine]);
end;

function TdxCssStyleCollection.GetCount: Integer;
begin
  if FStyleTable = nil then
    ParseString;
  Result := FStyleTable.Count;
  if FHtmlStyleTable <> nil then
    Inc(Result, FHtmlStyleTable.Count);
end;

function TdxCssStyleCollection.GetItem(const AKey: string): string;
var
  AStyleKey: TdxHtmlTextWriterStyle;
begin
  if FStyleTable = nil then
    ParseString;
  if not FStyleTable.TryGetValue(AKey, Result) then
  begin
    AStyleKey := TdxCssTextWriter.GetStyleKey(AKey);
    if AStyleKey <> TdxHtmlTextWriterStyle.Invalid then
      Result := GetItem(AStyleKey)
    else
      Result := '';
  end;
end;

function TdxCssStyleCollection.GetItem(const AKey: TdxHtmlTextWriterStyle): string;
begin
  if FHtmlStyleTable = nil then
    Exit('');
  if not FHtmlStyleTable.TryGetValue(AKey, Result) then
    Result := '';
end;

function TdxCssStyleCollection.GetKeys: TArray<string>;
var
  AIndex: Integer;
  S: string;
  AStyle: TdxHtmlTextWriterStyle;
begin
  if FStyleTable = nil then
    ParseString;
  if FHtmlStyleTable = nil then
    Exit(FStyleTable.Keys.ToArray);

  SetLength(Result, FStyleTable.Count + FHtmlStyleTable.Count);
  AIndex := 0;
  for S in FStyleTable.Keys do
  begin
    Result[AIndex] := S;
    Inc(AIndex)
  end;
  for AStyle in FHtmlStyleTable.Keys do
  begin
    Result[AIndex] := TdxCssTextWriter.GetStyleName(AStyle);
    Inc(AIndex);
  end;
end;

function TdxCssStyleCollection.GetValue: string;
begin
  if FState <> nil then
    Exit(FState[StyleKey].AsString);

  if FStyle = '' then
    FStyle := BuildString;

  Result := FStyle;
end;

procedure TdxCssStyleCollection.SetValue(const AValue: string);
begin
  if FState = nil then
    FStyle := AValue
  else
    FState[StyleKey] := AValue;
  FreeAndNil(FStyleTable);
end;

procedure TdxCssStyleCollection.Add(const AKey, AValue: string);
var
  AStyleKey: TdxHtmlTextWriterStyle;
begin
  if AKey = '' then
    raise EArgumentNilException.Create('key');
  if FStyleTable = nil then
    ParseString;
  FStyleTable.AddOrSetValue(AKey, AValue);
  if FHtmlStyleTable <> nil then
  begin
    AStyleKey := TdxCssTextWriter.GetStyleKey(AKey);
    if AStyleKey <> TdxHtmlTextWriterStyle.Invalid then
      FHtmlStyleTable.Remove(AStyleKey);
  end;
  if FState <> nil then
    FState.AddOrSetValue(StyleKey, BuildString);
  FStyle := '';
end;

procedure TdxCssStyleCollection.Add(AKey: TdxHtmlTextWriterStyle; const AValue: string);
var
  AStyleName: string;
begin
  if FHtmlStyleTable = nil then
    FHtmlStyleTable := TDictionary<TdxHtmlTextWriterStyle, string>.Create;
  FHtmlStyleTable.AddOrSetValue(AKey, AValue);
  AStyleName := TdxCssTextWriter.GetStyleName(AKey);
  if AStyleName <> '' then
  begin
    if FStyleTable = nil then
      ParseString;
    FStyleTable.Remove(AStyleName);
  end;
  if FState <> nil then
    FState[StyleKey] := BuildString;
  FStyle := '';
end;

procedure TdxCssStyleCollection.Clear;
begin
  FreeAndNil(FStyleTable);
  FreeAndNil(FHtmlStyleTable);
  if FState <> nil then
    FState.Remove(StyleKey);
  FStyle := '';
end;


procedure TdxCssStyleCollection.ParseString;
var
  AInput, AStyleName, AStyleValue: string;
  AMatch: TMatch;
  AStyleNames, AStyleValues: TGroup;
begin
  FreeAndNil(FStyleTable);
  FStyleTable := TdxStringsDictionary.Create(TdxStringComparer.Ordinal);
  if FState = nil then
    AInput := FStyle
  else
  begin
    if FState.ContainsKey(StyleKey) then
      AInput := FState[StyleKey].AsString
    else
      AInput := '';
  end;
  if AInput <> '' then
  begin
    AMatch := FStyleAttribRegex.Match(AInput, 0);
    while AMatch.Success do
    begin
{$IFDEF DELPHI102TOKYO}
      if AMatch.Groups.Count <= 1 then
        Break;
{$ENDIF}
      AStyleNames := AMatch.Groups['stylename'];
      AStyleValues := AMatch.Groups['styleval'];
      AStyleName := AStyleNames.Value;
      AStyleValue := AStyleValues.Value;
      FStyleTable.AddOrSetValue(AStyleName, AStyleValue);
      AMatch := AMatch.NextMatch;
    end;
  end;
end;

procedure TdxCssStyleCollection.Remove(const AKey: string);
begin
  if FStyleTable = nil then
    ParseString;
  if FStyleTable.ContainsKey(AKey) then
  begin
    FStyleTable.Remove(AKey);
    if FState <> nil then
      FState[StyleKey] := BuildString;
    FStyle := '';
  end;
end;

procedure TdxCssStyleCollection.Remove(AKey: TdxHtmlTextWriterStyle);
begin
  if FHtmlStyleTable <> nil then
  begin
    FHtmlStyleTable.Remove(AKey);
    if FState <> nil then
      FState[StyleKey] := BuildString;
    FStyle := '';
  end;
end;

procedure TdxCssStyleCollection.Render(AWriter: TdxCssTextWriter);
var
  AStyle: string;
  AHtmlStyle: TPair<TdxHtmlTextWriterStyle, string>;
begin
  if (FStyleTable <> nil) and (FStyleTable.Count > 0) then
    for AStyle in FStyleTable.Keys do
      AWriter.WriteAttribute(AStyle, FStyleTable[AStyle]);

  if (FHtmlStyleTable <> nil) and (FHtmlStyleTable.Count > 0) then
    for AHtmlStyle in FHtmlStyleTable do
      AWriter.WriteAttribute(AHtmlStyle.Key, AHtmlStyle.Value);
end;

procedure TdxCssStyleCollection.Render(AWriter: TdxHtmlTextWriter);
var
  AStyle: string;
  AHtmlStyle: TPair<TdxHtmlTextWriterStyle, string>;
begin
  if (FStyleTable <> nil) and (FStyleTable.Count > 0) then
    for AStyle in FStyleTable.Keys do
      AWriter.AddStyleAttribute(AStyle, FStyleTable[AStyle]);
  if (FHtmlStyleTable <> nil) and (FHtmlStyleTable.Count > 0) then
    for AHtmlStyle in FHtmlStyleTable do
      AWriter.AddStyleAttribute(AHtmlStyle.Key, AHtmlStyle.Value);
end;

function TdxCssStyleCollection.BuildString: string;
var
  AWriter: TStringWriter;
  ADxWriter: TdxCssTextWriter;
begin
  if ((FStyleTable = nil) or (FStyleTable.Count = 0)) and ((FHtmlStyleTable = nil) or (FHtmlStyleTable.Count = 0)) then
    Exit('');

  AWriter := TStringWriter.Create;
  try
    ADxWriter := TdxCssTextWriter.Create(AWriter);
    try
      Render(ADxWriter);
      Exit(AWriter.ToString);
    finally
      ADxWriter.Free;
    end
  finally
    AWriter.Free;
  end;
end;

{ TdxWebAttributeCollection }

constructor TdxWebAttributeCollection.Create(ABag: TdxStateBag);
begin
  inherited Create;
  FBag := ABag;
end;

destructor TdxWebAttributeCollection.Destroy;
begin
  FStyleCollection.Free;
  inherited Destroy;
end;

function TdxWebAttributeCollection.GetCount: Integer;
begin
  Result := FBag.Count;
end;

function TdxWebAttributeCollection.GetCssStyle: TdxCssStyleCollection;
begin
  if FStyleCollection = nil then
    FStyleCollection := TdxCssStyleCollection.Create(FBag);
  Result := FStyleCollection;
end;

function TdxWebAttributeCollection.GetItem(const AKey: string): TdxNullableString;
begin
  if (FStyleCollection <> nil) and SameText(AKey, 'style') then
    Result := FStyleCollection.Value
  else
    if FBag[AKey].IsEmpty then
      Result := TdxNullableString.Null
    else
      Result := FBag[AKey].AsString;
end;

function TdxWebAttributeCollection.GetKeys: TArray<string>;
begin
  Result := FBag.Keys.ToArray;
end;

procedure TdxWebAttributeCollection.SetItem(const AKey: string; const Value: TdxNullableString);
begin
  Add(AKey, Value);
end;

procedure TdxWebAttributeCollection.Add(const AKey: string; const Value: TdxNullableString);
begin
  if (FStyleCollection <> nil) and SameText(AKey, 'style') then
    FStyleCollection.Value := Value
  else
    if Value.HasValue then
      FBag.AddOrSetValue(AKey, Value.Value)
    else
      FBag.AddOrSetValue(AKey, TValue.Empty);
end;

procedure TdxWebAttributeCollection.AddAttributes(AWriter: TdxHtmlTextWriter);
var
  AItem: TValue;
  AKey: string;
begin
  if FBag.Count > 0 then
  begin
    for AKey in FBag.Keys do
    begin
      if AKey = '' then
        Continue;
      AItem := FBag[AKey];
      if not AItem.IsEmpty and AItem.IsString then
        AWriter.AddAttribute(AKey, Items[AKey], True);
    end;
  end;
end;

procedure TdxWebAttributeCollection.Clear;
begin
  FBag.Clear;
  if FStyleCollection <> nil then
    FStyleCollection.Clear;
end;

function TdxWebAttributeCollection.Equals(O: TObject): Boolean;
var
  AAttributes: TdxWebAttributeCollection;
  AKey: string;
begin
  AAttributes := Safe<TdxWebAttributeCollection>.Cast(O);
  if AAttributes = nil then
    Exit(False);
  if AAttributes.Count <> FBag.Count then
    Exit(False);
  for AKey in FBag.Keys.ToArray do
  begin
    if Self[AKey] <> AAttributes[AKey] then
      Exit(False);
  end;
  Result := True;
end;

procedure TdxWebAttributeCollection.Remove(const AKey: string);
begin
  if (FStyleCollection <> nil) and SameText(AKey, 'style') then
    FStyleCollection.Clear
  else
    FBag.Remove(AKey);
end;

procedure TdxWebAttributeCollection.Render(AWriter: TdxHtmlTextWriter);
var
  APair: string;
begin
  if FBag.Count = 0 then
    Exit;
  for APair in FBag.Keys.ToArray do
    if (APair <> '') and not FBag[APair].IsEmpty then
      AWriter.WriteAttribute(APair, FBag[APair].AsString, True);
end;

{ TdxHtmlControl }

constructor TdxHtmlControl.Create(ATagKey: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span);
begin
  inherited Create;
  FTagKey := ATagKey;
end;

destructor TdxHtmlControl.Destroy;
begin
  FControlStyle.Free;
  FAttributes.Free;
  inherited Destroy;
end;

class function TdxHtmlControl.MapIntegerAttributeToString(N: Integer): string;
begin
  if N = -1 then
    Result := ''
  else
    Result := IntToStr(N);
end;

class function TdxHtmlControl.MapStringAttributeToString(const S: string): string;
begin
  Result := S;
end;

function TdxHtmlControl.GetAttributes: TdxWebAttributeCollection;
begin
  if FAttributes = nil then
    FAttributes := TdxWebAttributeCollection.Create(ViewState);
  Result := FAttributes;
end;

function TdxHtmlControl.GetBorderWidth: TdxWebUnit;
begin
  if not ControlStyleCreated then
    Exit(TdxWebUnit.Empty);
  Result := ControlStyle.BorderWidth;
end;

procedure TdxHtmlControl.SetBorderWidth(const AValue: TdxWebUnit);
begin
  ControlStyle.BorderWidth := AValue;
end;

function TdxHtmlControl.GetRequiresLegacyRendering: Boolean;
begin
  Result := False;
end;

function TdxHtmlControl.GetDisabled: Boolean;
var
  AStr: string;
begin
  AStr := Attributes['disabled'];
  Result := AStr = 'disabled';
end;

procedure TdxHtmlControl.SetDisabled(const AValue: Boolean);
begin
  if AValue then
    Attributes['disabled'] := 'disabled'
  else
    Attributes['disabled'] := '';
end;

function TdxHtmlControl.GetStyle: TdxCssStyleCollection;
begin
  Result := Attributes.CssStyle;
end;

function TdxHtmlControl.GetWidth: TdxWebUnit;
begin
  if not ControlStyleCreated then
    Exit(TdxWebUnit.Empty);
  Result := ControlStyle.Width;
end;

procedure TdxHtmlControl.SetWidth(const AValue: TdxWebUnit);
begin
  ControlStyle.Width := AValue;
end;

function TdxHtmlControl.GetHeight: TdxWebUnit;
begin
  if not ControlStyleCreated then
    Exit(TdxWebUnit.Empty);
  Result := ControlStyle.Height;
end;

procedure TdxHtmlControl.SetHeight(const AValue: TdxWebUnit);
begin
  ControlStyle.Height := AValue;
end;

function TdxHtmlControl.GetFont: TdxWebFontInfo;
begin
  Result := ControlStyle.Font;
end;

function TdxHtmlControl.GetCssClass: string;
begin
  if not ControlStyleCreated then
    Exit('');
  Result := ControlStyle.CssClass;
end;

procedure TdxHtmlControl.SetCssClass(const AValue: string);
begin
  ControlStyle.CssClass := AValue;
end;

function TdxHtmlControl.GetControlStyleCreated: Boolean;
begin
  Result := FControlStyle <> nil;
end;

function TdxHtmlControl.GetControlStyle: TdxWebStyle;
begin
  if FControlStyle = nil then
    FControlStyle := CreateControlStyle;
  Result := FControlStyle;
end;

function TdxHtmlControl.GetBorderStyle: TdxWebBorderStyle;
begin
  if not ControlStyleCreated then
    Result := TdxWebBorderStyle.NotSet
  else
    Result := ControlStyle.BorderStyle;
end;

procedure TdxHtmlControl.SetBorderStyle(const AValue: TdxWebBorderStyle);
begin
  ControlStyle.BorderStyle := AValue;
end;

function TdxHtmlControl.GetEnabled: Boolean;
begin
  Result := not Flags[$80000];
end;

procedure TdxHtmlControl.SetEnabled(const AValue: Boolean);
var
  AFlag: Boolean;
begin
  AFlag := not Flags[$80000];
  if AFlag <> AValue then
  begin
    if not AValue then
      Flags.&Set($80000)
    else
      Flags.Clear($80000);
    if IsTrackingViewState then
      FWebControlFlags.&Set(2);
  end;
end;

function TdxHtmlControl.GetToolTip: string;
begin
  if FWebControlFlags[8] then
    Result := ViewState['ToolTip'].AsString
  else
    Result := '';
end;

procedure TdxHtmlControl.SetToolTip(const AValue: string);
begin
  ViewState['ToolTip'] := AValue;
  FWebControlFlags.&Set(8);
end;

function TdxHtmlControl.GetTagName: string;
const
  ResultMap: array[TdxHtmlTextWriterTag] of string = (
    'unknown', 'a', 'acronym', 'address', 'area', 'b', 'base', 'basefont', 'bdo',
    'bgsound', 'big', 'blockquote', 'body', 'br', 'button', 'caption', 'center', 'cite',
    'code', 'col', 'colgroup', 'dd', 'del', 'dfn', 'dir', 'div', 'dl', 'dt', 'em', 'embed',
    'fieldset', 'font', 'form', 'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head',
    'hr', 'html', 'i', 'iframe', 'img', 'input', 'ins', 'isindex', 'kbd', 'label',
    'legend', 'li', 'link', 'map', 'marquee', 'menu', 'meta', 'nobr', 'noframes',
    'noscript', 'object', 'ol', 'option', 'p', 'param', 'pre', 'q', 'rt', 'ruby',
    's', 'samp', 'script', 'select', 'small', 'span', 'strike', 'strong', 'style', 'sub',
    'sup', 'table', 'tbody', 'td', 'textarea', 'tfoot', 'th', 'thead', 'title', 'tr',
    'tt', 'u', 'ul', 'var', 'wbr', 'xml');

begin
  Result := ResultMap[FTagKey];
end;

function TdxHtmlControl.ToString: string;
begin
  Result := Format('%s <%s>', [ClassName, GetTagName]);
end;

procedure TdxHtmlControl.AddDisplayInlineBlockIfNeeded(AWriter: TdxHtmlTextWriter);
begin
  if not RequiresLegacyRendering and
      ((BorderStyle <> TdxWebBorderStyle.NotSet) or not BorderWidth.IsEmpty or
        not Height.IsEmpty or not Width.IsEmpty) then
    AWriter.AddStyleAttribute(TdxHtmlTextWriterStyle.Display, 'inline-block');
end;

procedure TdxHtmlControl.Render(AWriter: TdxHtmlTextWriter);
begin
  RenderBeginTag(AWriter);
end;

function TdxHtmlControl.CreateControlCollection: TdxWebControlCollection;
begin
  Result := TdxWebEmptyControlCollection.Create(Self);
end;

function TdxHtmlControl.GetAttribute(const AName: string): string;
begin
  Result := Attributes[AName];
end;

procedure TdxHtmlControl.RenderBeginTag(AWriter: TdxHtmlTextWriter);
begin
  AddAttributesToRender(AWriter);
  AWriter.RenderBeginTag(TagKey);
end;

procedure TdxHtmlControl.AddAttributesToRender(AWriter: TdxHtmlTextWriter);
var
  AToolTip, AAttributeName, AKey: string;
begin
  if ID <> '' then
    AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Id, ClientID);
  if not Enabled then
    AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Disabled, 'disabled');
  if FWebControlFlags[8] then
  begin
    AToolTip := ToolTip;
    if Length(AToolTip) > 0 then
      AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Title, AToolTip);
  end;
  if (TagKey = TdxHtmlTextWriterTag.Span) or (TagKey = TdxHtmlTextWriterTag.A) then
    AddDisplayInlineBlockIfNeeded(AWriter);
  if ControlStyleCreated and not ControlStyle.IsEmpty then
    ControlStyle.AddAttributesToRender(AWriter, Self);

  for AAttributeName in Attributes.Keys do
    if not SameText(AAttributeName, 'style') then
      AWriter.AddAttribute(AAttributeName, Attributes[AAttributeName]);
  for AKey in Style.Keys do
    AWriter.AddStyleAttribute(AKey, Style[AKey]);
end;

procedure TdxHtmlControl.SetAttribute(const AName: string; const AValue: string);
begin
  Attributes[AName] := AValue;
end;

function TdxHtmlControl.CreateControlStyle: TdxWebStyle;
begin
  Result := TdxWebStyle.Create(ViewState);
end;

{ TdxWebStyle }

constructor TdxWebStyle.Create(ABag: TdxStateBag = nil);
begin
  inherited Create;
  FStateBag := ABag;
  FOwnStateBag := ABag = nil;
end;

destructor TdxWebStyle.Destroy;
begin
  FFontInfo.Free;
  if FOwnStateBag then
    FStateBag.Free;
  inherited Destroy;
end;

class function TdxWebStyle.FormatStringArray(const AArray: TArray<string>; ADelimiter: Char): string;
begin
  case Length(AArray) of
    1:
      Exit(AArray[0]);
    0:
      Exit('');
  end;
  Result := TdxStringHelper.Join(ADelimiter, AArray);
end;

function TdxWebStyle.GetBackColor: TdxAlphaColor;
begin
  if IsSet(TAssignedValue.BackColor) then
    Result := ViewState['BackColor'].AsType<TdxAlphaColor>
  else
    Result := TdxAlphaColors.Empty;
end;

procedure TdxWebStyle.SetBackColor(const AValue: TdxAlphaColor);
begin
  ViewState['BackColor'] := AValue;
  SetBit(TAssignedValue.BackColor);
end;

function TdxWebStyle.GetBorderColor: TdxAlphaColor;
begin
  if IsSet(TAssignedValue.BorderColor) then
    Result := ViewState['BorderColor'].AsType<TdxAlphaColor>
  else
    Result := TdxAlphaColors.Empty;
end;

procedure TdxWebStyle.SetBorderColor(const AValue: TdxAlphaColor);
begin
  ViewState['BorderColor'] := AValue;
  SetBit(TAssignedValue.BorderColor);
end;

function TdxWebStyle.GetBorderStyle: TdxWebBorderStyle;
begin
  if IsSet(TAssignedValue.BorderStyle) then
    Result := ViewState['BorderStyle'].AsType<TdxWebBorderStyle>
  else
    Result := TdxWebBorderStyle.NotSet;
end;

procedure TdxWebStyle.SetBorderStyle(const AValue: TdxWebBorderStyle);
begin
  ViewState['BorderStyle'] := TValue.From(AValue);
  SetBit(TAssignedValue.BorderStyle);
end;

function TdxWebStyle.GetBorderWidth: TdxWebUnit;
begin
  if IsSet(TAssignedValue.BorderWidth) then
    Result := ViewState['BorderWidth'].AsType<TdxWebUnit>
  else
    Result := TdxWebUnit.Empty;
end;

procedure TdxWebStyle.SetBorderWidth(const AValue: TdxWebUnit);
begin
  if (AValue.&Type = TdxWebUnitType.Percentage) or (AValue.Value < 0.0) then
    raise EArgumentOutOfRangeException.Create('value');
  ViewState['BorderWidth'] := TValue.From(AValue);
  SetBit(TAssignedValue.BorderWidth);
end;

function TdxWebStyle.GetCssClass: string;
begin
  if IsSet(TAssignedValue.CssClass) then
    Result := ViewState[CssClassStr].AsString
  else
    Result := '';
end;

procedure TdxWebStyle.SetCssClass(const AValue: string);
begin
  ViewState.AddOrSetValue(CssClassStr, AValue);
  SetBit(TAssignedValue.CssClass);
end;

function TdxWebStyle.GetFont: TdxWebFontInfo;
begin
  if FFontInfo = nil then
    FFontInfo := TdxWebFontInfo.Create(Self);
  Result := FFontInfo;
end;

function TdxWebStyle.GetForeColor: TdxAlphaColor;
begin
  if IsSet(TAssignedValue.ForeColor) then
    Result := ViewState['ForeColor'].AsType<TdxAlphaColor>
  else
    Result := TdxAlphaColors.Empty;
end;

procedure TdxWebStyle.SetForeColor(const AValue: TdxAlphaColor);
begin
  ViewState['ForeColor'] := AValue;
  SetBit(TAssignedValue.ForeColor);
end;

function TdxWebStyle.GetHeight: TdxWebUnit;
begin
  if IsSet(TAssignedValue.Height) then
    Result := ViewState['Height'].AsType<TdxWebUnit>
  else
    Result := TdxWebUnit.Empty;
end;

procedure TdxWebStyle.SetHeight(const AValue: TdxWebUnit);
begin
  if AValue.Value < 0.0 then
    raise EArgumentOutOfRangeException.Create('AValue');
  ViewState['Height'] := TValue.From(AValue);
  SetBit(TAssignedValue.Height);
end;

function TdxWebStyle.GetIsEmpty: Boolean;
begin
  Result := (FAssignedValues = []) and (Length(RegisteredCssClass) = 0);
end;

function TdxWebStyle.GetRegisteredCssClass: string;
begin
  Result := FRegisteredCssClass;
end;

function TdxWebStyle.GetWidth: TdxWebUnit;
begin
  if IsSet(TAssignedValue.Width) then
    Result := ViewState['Width'].AsType<TdxWebUnit>
  else
    Result := TdxWebUnit.Empty;
end;

procedure TdxWebStyle.SetWidth(const AValue: TdxWebUnit);
begin
  if AValue.Value < 0.0 then
    raise EArgumentOutOfRangeException.Create('value');
  ViewState['Width'] := TValue.From(AValue);
  SetBit(TAssignedValue.Width);
end;

function TdxWebStyle.GetViewState: TdxStateBag;
begin
  if FStateBag = nil then
    FStateBag := TdxStateBag.Create;
  Result := FStateBag;
end;

procedure TdxWebStyle.AddAttributesToRender(AWriter: TdxHtmlTextWriter; AOwner: TdxHtmlControl = nil);
var
  ARegisteredCssClass: string;
  AStyleAttributes: TdxCssStyleCollection;
  AFlag: Boolean;
begin
  ARegisteredCssClass := '';
  AFlag := True;
  if IsSet(TAssignedValue.CssClass) then
    ARegisteredCssClass := ViewState[CssClassStr].AsString;
  if FRegisteredCssClass <> '' then
  begin
    AFlag := False;
    if Length(ARegisteredCssClass) <> 0 then
      ARegisteredCssClass := Format('%s %s', [ARegisteredCssClass, FRegisteredCssClass])
    else
      ARegisteredCssClass := FRegisteredCssClass;
  end;
  if Length(ARegisteredCssClass) > 0 then
    AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Class, ARegisteredCssClass);
  if AFlag then
  begin
    AStyleAttributes := GetStyleAttributes;
    try
      AStyleAttributes.Render(AWriter);
    finally
      AStyleAttributes.Free;
    end;
  end;
end;

procedure TdxWebStyle.CopyFrom(S: TdxWebStyle);
begin
  if Length(RegisteredCssClass) <> 0 then
    TdxRichEditExceptions.ThrowInvalidOperationException('Style_RegisteredStylesAreReadOnly');
  if (S <> nil) and not S.IsEmpty then
  begin
    Font.CopyFrom(S.Font);
    if S.IsSet(TAssignedValue.CssClass) then
      CssClass := S.CssClass;
    if Length(S.RegisteredCssClass) <> 0 then
    begin
      if IsSet(TAssignedValue.CssClass) then
        CssClass := Format('%s %s', [CssClass, S.RegisteredCssClass])
      else
        CssClass := S.RegisteredCssClass;
      if S.IsSet(TAssignedValue.BackColor) and (S.BackColor <> TdxAlphaColors.Empty) then
      begin
        ViewState.Remove('BackColor');
        ClearBit(TAssignedValue.BackColor);
      end;
      if S.IsSet(TAssignedValue.ForeColor) and (S.ForeColor <> TdxAlphaColors.Empty) then
      begin
        ViewState.Remove('ForeColor');
        ClearBit(TAssignedValue.ForeColor);
      end;
      if S.IsSet(TAssignedValue.BorderColor) and (S.BorderColor <> TdxAlphaColors.Empty) then
      begin
        ViewState.Remove('BorderColor');
        ClearBit(TAssignedValue.BorderColor);
      end;
      if S.IsSet(TAssignedValue.BorderWidth) and (S.BorderWidth <> TdxWebUnit.Empty) then
      begin
        ViewState.Remove('BorderWidth');
        ClearBit(TAssignedValue.BorderWidth);
      end;
      if S.IsSet(TAssignedValue.BorderStyle) then
      begin
        ViewState.Remove('BorderStyle');
        ClearBit(TAssignedValue.BorderStyle);
      end;
      if S.IsSet(TAssignedValue.Height) and (S.Height <> TdxWebUnit.Empty) then
      begin
        ViewState.Remove('Height');
        ClearBit(TAssignedValue.Height);
      end;
      if S.IsSet(TAssignedValue.Width) and (S.Width <> TdxWebUnit.Empty) then
      begin
        ViewState.Remove('Width');
        ClearBit(TAssignedValue.Width);
      end;
    end
    else
    begin
      if S.IsSet(TAssignedValue.BackColor) and (S.BackColor <> TdxAlphaColors.Empty) then
        BackColor := S.BackColor;
      if S.IsSet(TAssignedValue.ForeColor) and (S.ForeColor <> TdxAlphaColors.Empty) then
        ForeColor := S.ForeColor;
      if S.IsSet(TAssignedValue.BorderColor) and (S.BorderColor <> TdxAlphaColors.Empty) then
        BorderColor := S.BorderColor;
      if S.IsSet(TAssignedValue.BorderWidth) and (S.BorderWidth <> TdxWebUnit.Empty) then
        BorderWidth := S.BorderWidth;
      if S.IsSet(TAssignedValue.BorderStyle) then
        BorderStyle := S.BorderStyle;
      if S.IsSet(TAssignedValue.Height) and (S.Height <> TdxWebUnit.Empty) then
        Height := S.Height;
      if S.IsSet(TAssignedValue.Width) and (S.Width <> TdxWebUnit.Empty) then
        Width := S.Width;
    end;
  end;
end;

function TdxWebStyle.GetStyleAttributes: TdxCssStyleCollection;
begin
  Result := TdxCssStyleCollection.Create;
  FillStyleAttributes(Result);
end;

procedure TdxWebStyle.MergeWith(S: TdxWebStyle);
begin
  if Length(RegisteredCssClass) <> 0 then
    TdxRichEditExceptions.ThrowInvalidOperationException('Style_RegisteredStylesAreReadOnly');
  if (S <> nil) and not S.IsEmpty then
  begin
    if IsEmpty then
      CopyFrom(S)
    else
    begin
      Font.MergeWith(S.Font);
      if S.IsSet(TAssignedValue.CssClass) and not IsSet(TAssignedValue.CssClass) then
        CssClass := S.CssClass;
      if Length(S.RegisteredCssClass) = 0 then
      begin
        if S.IsSet(TAssignedValue.BackColor) and (not IsSet(TAssignedValue.BackColor) or (BackColor = TdxAlphaColors.Empty)) then
          BackColor := S.BackColor;
        if S.IsSet(TAssignedValue.ForeColor) and (not IsSet(TAssignedValue.ForeColor) or (ForeColor = TdxAlphaColors.Empty)) then
          ForeColor := S.ForeColor;
        if S.IsSet(TAssignedValue.BorderColor) and (not IsSet(TAssignedValue.BorderColor) or (BorderColor = TdxAlphaColors.Empty)) then
          BorderColor := S.BorderColor;
        if S.IsSet(TAssignedValue.BorderWidth) and (not IsSet(TAssignedValue.BorderWidth) or (BorderWidth = TdxWebUnit.Empty)) then
          BorderWidth := S.BorderWidth;
        if S.IsSet(TAssignedValue.BorderStyle) and not IsSet(TAssignedValue.BorderStyle) then
          BorderStyle := S.BorderStyle;
        if S.IsSet(TAssignedValue.Height) and (not IsSet(TAssignedValue.Height) or (Height = TdxWebUnit.Empty)) then
          Height := S.Height;
        if S.IsSet(TAssignedValue.Width) and (not IsSet(TAssignedValue.Width) or (Width = TdxWebUnit.Empty)) then
          Width := S.Width;
      end
      else
        if IsSet(TAssignedValue.CssClass) then
          CssClass := Format('%s %s', [CssClass, S.RegisteredCssClass])
        else
          CssClass := S.RegisteredCssClass;
    end;
  end;
end;

procedure TdxWebStyle.Reset;
begin
  if FStateBag <> nil then
  begin
    if IsSet(TAssignedValue.CssClass) then
      ViewState.Remove(CssClassStr);
    if IsSet(TAssignedValue.BackColor) then
      ViewState.Remove('BackColor');
    if IsSet(TAssignedValue.ForeColor) then
      ViewState.Remove('ForeColor');
    if IsSet(TAssignedValue.BorderColor) then
      ViewState.Remove('BorderColor');
    if IsSet(TAssignedValue.BorderWidth) then
      ViewState.Remove('BorderWidth');
    if IsSet(TAssignedValue.BorderStyle) then
      ViewState.Remove('BorderStyle');
    if IsSet(TAssignedValue.Height) then
      ViewState.Remove('Height');
    if IsSet(TAssignedValue.Width) then
      ViewState.Remove('Width');
    Font.Reset;
    ViewState.Remove('_!SB');
    FMarkedAssignedValues := [];
  end;
  FAssignedValues := [];
end;

procedure TdxWebStyle.SetRegisteredCssClass(const ACssClass: string);
begin
  FRegisteredCssClass := ACssClass;
end;

function TdxWebStyle.IsSet(AValue: TAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxWebStyle.ClearBit(AValue: TAssignedValue);
begin
  Exclude(FAssignedValues, AValue);
end;

procedure TdxWebStyle.FillStyleAttributes(AAttributes: TdxCssStyleCollection);
var
  AColor: TdxAlphaColor;
  AUnit, ABorderWidth: TdxWebUnit;
  ABorderStyle: TdxWebBorderStyle;
  AFont: TdxWebFontInfo;
  ANames: TArray<string>;
  ASize: TdxWebFontUnit;
  S: string;
begin
  if IsSet(TAssignedValue.ForeColor) then
  begin
    AColor := ViewState['ForeColor'].AsType<TdxAlphaColor>;
    if not TdxAlphaColors.IsEmpty(AColor) then
      AAttributes.Add(TdxHtmlTextWriterStyle.Color, TdxAlphaColors.ToHtml(AColor));
  end;
  if IsSet(TAssignedValue.BackColor) then
  begin
    AColor := ViewState['BackColor'].AsType<TdxAlphaColor>;
    if not TdxAlphaColors.IsEmpty(AColor) then
      AAttributes.Add(TdxHtmlTextWriterStyle.BackgroundColor, TdxAlphaColors.ToHtml(AColor));
  end;
  if IsSet(TAssignedValue.BorderColor) then
  begin
    AColor := ViewState['BorderColor'].AsType<TdxAlphaColor>;
    if not TdxAlphaColors.IsEmpty(AColor) then
      AAttributes.Add(TdxHtmlTextWriterStyle.BorderColor, TdxAlphaColors.ToHtml(AColor));
  end;
  ABorderStyle := BorderStyle;
  ABorderWidth := BorderWidth;
  if not ABorderWidth.IsEmpty then
  begin
    AAttributes.Add(TdxHtmlTextWriterStyle.BorderWidth, ABorderWidth.ToString(TdxCultureInfo.InvariantCulture));
    if ABorderStyle = TdxWebBorderStyle.NotSet then
    begin
      if ABorderWidth.Value <> 0.0 then
        AAttributes.Add(TdxHtmlTextWriterStyle.BorderStyle, 'solid');
    end
    else
      AAttributes.Add(TdxHtmlTextWriterStyle.BorderStyle, BorderStyles[Integer(ABorderStyle)]);
  end
  else
    if ABorderStyle <> TdxWebBorderStyle.NotSet then
      AAttributes.Add(TdxHtmlTextWriterStyle.BorderStyle, BorderStyles[Integer(ABorderStyle)]);
  AFont := Font;
  ANames := AFont.Names;
  if Length(ANames) > 0 then
    AAttributes.Add(TdxHtmlTextWriterStyle.FontFamily, FormatStringArray(ANames, ','));
  ASize := AFont.Size;
  if not ASize.IsEmpty then
    AAttributes.Add(TdxHtmlTextWriterStyle.FontSize, ASize.ToString(TdxCultureInfo.InvariantCulture));
  if IsSet(TAssignedValue.FontBold) then
  begin
    if AFont.Bold then
      AAttributes.Add(TdxHtmlTextWriterStyle.FontWeight, 'bold')
    else
      AAttributes.Add(TdxHtmlTextWriterStyle.FontWeight, 'normal');
  end;
  if IsSet(TAssignedValue.FontItalic) then
  begin
    if AFont.Italic then
      AAttributes.Add(TdxHtmlTextWriterStyle.FontStyle, 'italic')
    else
      AAttributes.Add(TdxHtmlTextWriterStyle.FontStyle, 'normal');
  end;
  S := '';
  if AFont.Underline then
    S := 'underline';
  if AFont.Overline then
    S := S + ' overline';
  if AFont.Strikeout then
    S := S + ' line-through';
  if S <> '' then
    AAttributes.Add(TdxHtmlTextWriterStyle.TextDecoration, S)
  else
    if IsSet(TAssignedValue.FontUnderline) or IsSet(TAssignedValue.FontOverline) or IsSet(TAssignedValue.FontStrikeout) then
      AAttributes.Add(TdxHtmlTextWriterStyle.TextDecoration, 'none');
  if IsSet(TAssignedValue.Height) then
  begin
    AUnit := ViewState['Height'].AsType<TdxWebUnit>;
    if not AUnit.IsEmpty then
      AAttributes.Add(TdxHtmlTextWriterStyle.Height, AUnit.ToString(TdxCultureInfo.InvariantCulture));
  end;
  if IsSet(TAssignedValue.Width) then
  begin
    AUnit := ViewState['Width'].AsType<TdxWebUnit>;
    if not AUnit.IsEmpty then
      AAttributes.Add(TdxHtmlTextWriterStyle.Width, AUnit.ToString(TdxCultureInfo.InvariantCulture));
  end;
end;

procedure TdxWebStyle.SetBit(AValue: TAssignedValue);
begin
  Include(FAssignedValues, AValue);
  if IsTrackingViewState then
    Include(FMarkedAssignedValues, AValue);
end;

{ TdxHtmlStyleRender }

class function TdxHtmlStyleRender.GetHtmlStyle(AFont: TdxGPFont; AForeColor, ABackColor: TdxAlphaColor): string;
begin
  Result := Format('color:%s;background-color:%s;%s',
    [TdxHtmlConvert.ToHtml(AForeColor), TdxHtmlConvert.ToHtml(ABackColor), GetFontHtml(AFont)]);
end;

class function TdxHtmlStyleRender.GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
  ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor): string;
begin
  Result := GetHtmlStyle(AFontFamilyName, ASize, AUnit, ABold, AItalic, AStrikeout, AUnderline, AForeColor, ABackColor, False);
end;

class function TdxHtmlStyleRender.GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
  ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor; AUseFontSizeInPixels: Boolean): string;
var
  AFontHtml: string;
begin
  AFontHtml := GetFontHtml(AFontFamilyName, ASize, AUnit, ABold, AItalic, AStrikeout, AUnderline, AUseFontSizeInPixels);
  Result := Format('color:%s;background-color:%s;%s',
    [TdxHtmlConvert.ToHtml(AForeColor), TdxHtmlConvert.ToHtml(ABackColor), AFontHtml]);
end;

class procedure TdxHtmlStyleRender.GetHtmlStyle(AFont: TdxGPFont; AForeColor, ABackColor: TdxAlphaColor; AStyle: TdxCssStyleCollection);
begin
  AStyle.Add('color', TdxHtmlConvert.ToHtml(AForeColor));
  AStyle.Add('background-color', TdxHtmlConvert.ToHtml(ABackColor));
  GetFontHtml(AFont, AStyle);
end;

class procedure TdxHtmlStyleRender.GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
  ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor; AStyle: TdxCssStyleCollection);
begin
  GetHtmlStyle(AFontFamilyName, ASize, AUnit, ABold, AItalic, AStrikeout, AUnderline, AForeColor, ABackColor, False, AStyle);
end;

class procedure TdxHtmlStyleRender.GetHtmlStyle(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
  ABold, AItalic, AStrikeout, AUnderline: Boolean; AForeColor, ABackColor: TdxAlphaColor; AUseFontSizeInPixels: Boolean; AStyle: TdxCssStyleCollection);
begin
  AStyle.Add('color', TdxHtmlConvert.ToHtml(AForeColor));
  AStyle.Add('background-color', TdxHtmlConvert.ToHtml(ABackColor));
  GetFontHtml(AFontFamilyName, ASize, AUnit, ABold, AItalic, AStrikeout, AUnderline, AUseFontSizeInPixels, AStyle);
end;

class function TdxHtmlStyleRender.GetHtmlStyle(AFont: TdxGPFont; AForeColor: TdxAlphaColor): string;
begin
  Result := Format('color:%s;%s', [TdxHtmlConvert.ToHtml(AForeColor), GetFontHtml(AFont)]);
end;

class function TdxHtmlStyleRender.GetFontHtml(AFont: TdxGPFont): string;
begin
//  if AFont = nil then
//    Result := ''
//  else
//    Result := GetFontHtml(FontHelper.GetFamilyName(AFont), AFont.Size, AFont.Unit, AFont.Bold, AFont.Italic, AFont.Strikeout, AFont.Underline);
NotImplemented;
end;

class function TdxHtmlStyleRender.GetFontHtmlInPixels(AFont: TdxGPFont): TObject;
begin
//  if AFont = nil then
//    Result := ''
//  else
//    Result := GetFontHtml(FontHelper.GetFamilyName(AFont), AFont.Size, AFont.Unit, AFont.Bold, AFont.Italic, AFont.Strikeout, AFont.Underline, True);
  NotImplemented;
  Result := nil;
end;

class function TdxHtmlStyleRender.GetFontHtml(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
  ABold, AItalic, AStrikeout, AUnderline: Boolean; AInPixels: Boolean): string;
var
  AFontString: string;
begin
  if AInPixels then
    AFontString := TdxHtmlConvert.FontSizeToStringInPixels(ASize, AUnit)
  else
    AFontString := TdxHtmlConvert.FontSizeToString(ASize, AUnit);
  Result := Format('font-family:%s; font-size:%s; font-weight:%s; font-style:%s; %s',
    [GetCorrectedFamilyName(AFontFamilyName), AFontString, GetFontWeight(ABold), GetFontStyle(AItalic), GetTextDecoration(AStrikeout, AUnderline)]);
end;

class function TdxHtmlStyleRender.GetFontHtml(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
  ABold, AItalic, AStrikeout, AUnderline: Boolean): string;
begin
  Result := GetFontHtml(AFontFamilyName, ASize, AUnit, ABold, AItalic, AStrikeout, AUnderline, False);
end;

class procedure TdxHtmlStyleRender.GetFontHtml(AFont: TdxGPFont; AStyle: TdxCssStyleCollection);
begin
//  if AFont = nil then
//    Exit;
//  GetFontHtml(TdxFontHelper.GetFamilyName(AFont), AFont.Size, AFont.Unit, AFont.Bold, AFont.Italic, AFont.Strikeout, AFont.Underline, False, AStyle);
NotImplemented;
end;

class procedure TdxHtmlStyleRender.GetFontHtml(const AFontFamilyName: string; ASize: Single; AUnit: TdxGraphicsUnit;
  ABold, AItalic, AStrikeout, AUnderline: Boolean; AInPixels: Boolean; AStyle: TdxCssStyleCollection);
var
  AFontString, AValue: string;
begin
  if AInPixels then
    AFontString := TdxHtmlConvert.FontSizeToStringInPixels(ASize, AUnit)
  else
    AFontString := TdxHtmlConvert.FontSizeToString(ASize, AUnit);

  AStyle.Add('font-family', GetCorrectedFamilyName(AFontFamilyName));
  AStyle.Add('font-size', AFontString);
  AStyle.Add('font-weight', GetFontWeight(ABold));
  AStyle.Add('font-style', GetFontStyle(AItalic));
  AValue := GetTextDecorationValue(AStrikeout, AUnderline);
  if AValue <> '' then
    AStyle.Add('text-decoration', AValue);
end;

class function TdxHtmlStyleRender.GetCorrectedFamilyName(const AFamilyName: string): string;
begin
  if TRegEx.IsMatch(AFamilyName, '\s\d') then
    Result := #$27 + AFamilyName + #$27
  else
    Result := AFamilyName;
end;

class function TdxHtmlStyleRender.GetFontWeight(AIsBold: Boolean): string;
begin
  if AIsBold then
    Result := 'bold'
  else
    Result := 'normal';
end;

class function TdxHtmlStyleRender.GetFontStyle(AIsItalic: Boolean): string;
begin
  if AIsItalic then
    Result := 'italic'
  else
    Result := 'normal';
end;

class function TdxHtmlStyleRender.GetTextDecoration(AStrikeout, AUnderline: Boolean): string;
var
  ABuilder: TStringBuilder;
  AValue: string;
begin
  ABuilder := TStringBuilder.Create;
  try
    AValue := GetTextDecorationValue(AStrikeout, AUnderline);
    if AValue <> '' then
    begin
      ABuilder.Append('text-decoration:');
      ABuilder.Append(AValue);
      ABuilder.Append(';');
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class function TdxHtmlStyleRender.GetTextDecorationValue(AStrikeout, AUnderline: Boolean): string;
begin
  if AStrikeout then
    Result := ' line-through'
  else
    Result := '';
  if AUnderline then
    Result := Result + ' underline';
end;

{ TdxCssTextWriter.TAttributeInformation }

constructor TdxCssTextWriter.TAttributeInformation.Create(const AName: string; AEncode, AIsUrl: Boolean);
begin
  Name := AName;
  Encode := AEncode;
  IsUrl := AIsUrl;
end;

{ TdxCssTextWriter }

constructor TdxCssTextWriter.Create(AWriter: TTextWriter);
begin
  inherited Create;
  FWriter := AWriter;
end;

class constructor TdxCssTextWriter.Initialize;
begin
  FStyleLookup := TdxNamedOrdinalDictionary<TdxHtmlTextWriterStyle>.Create;

  RegisterAttribute('', TdxHtmlTextWriterStyle.Invalid, True, False);

  RegisterAttribute('background-color', TdxHtmlTextWriterStyle.BackgroundColor);
  RegisterAttribute('background-image', TdxHtmlTextWriterStyle.BackgroundImage, True, True);
  RegisterAttribute('border-collapse', TdxHtmlTextWriterStyle.BorderCollapse);
  RegisterAttribute('border-color', TdxHtmlTextWriterStyle.BorderColor);
  RegisterAttribute('border-style', TdxHtmlTextWriterStyle.BorderStyle);
  RegisterAttribute('border-width', TdxHtmlTextWriterStyle.BorderWidth);
  RegisterAttribute('color', TdxHtmlTextWriterStyle.Color);
  RegisterAttribute('cursor', TdxHtmlTextWriterStyle.Cursor);
  RegisterAttribute('direction', TdxHtmlTextWriterStyle.Direction);
  RegisterAttribute('display', TdxHtmlTextWriterStyle.Display);
  RegisterAttribute('filter', TdxHtmlTextWriterStyle.Filter);
  RegisterAttribute('font-family', TdxHtmlTextWriterStyle.FontFamily, True);
  RegisterAttribute('font-size', TdxHtmlTextWriterStyle.FontSize);
  RegisterAttribute('font-style', TdxHtmlTextWriterStyle.FontStyle);
  RegisterAttribute('font-variant', TdxHtmlTextWriterStyle.FontVariant);
  RegisterAttribute('font-weight', TdxHtmlTextWriterStyle.FontWeight);
  RegisterAttribute('height', TdxHtmlTextWriterStyle.Height);
  RegisterAttribute('left', TdxHtmlTextWriterStyle.Left);
  RegisterAttribute('list-style-image', TdxHtmlTextWriterStyle.ListStyleImage, True, True);
  RegisterAttribute('list-style-type', TdxHtmlTextWriterStyle.ListStyleType);
  RegisterAttribute('margin', TdxHtmlTextWriterStyle.Margin);
  RegisterAttribute('margin-bottom', TdxHtmlTextWriterStyle.MarginBottom);
  RegisterAttribute('margin-left', TdxHtmlTextWriterStyle.MarginLeft);
  RegisterAttribute('margin-right', TdxHtmlTextWriterStyle.MarginRight);
  RegisterAttribute('margin-top', TdxHtmlTextWriterStyle.MarginTop);
  RegisterAttribute('overflow-x', TdxHtmlTextWriterStyle.OverflowX);
  RegisterAttribute('overflow-y', TdxHtmlTextWriterStyle.OverflowY);
  RegisterAttribute('overflow', TdxHtmlTextWriterStyle.Overflow);
  RegisterAttribute('padding', TdxHtmlTextWriterStyle.Padding);
  RegisterAttribute('padding-bottom', TdxHtmlTextWriterStyle.PaddingBottom);
  RegisterAttribute('padding-left', TdxHtmlTextWriterStyle.PaddingLeft);
  RegisterAttribute('padding-right', TdxHtmlTextWriterStyle.PaddingRight);
  RegisterAttribute('padding-top', TdxHtmlTextWriterStyle.PaddingTop);
  RegisterAttribute('position', TdxHtmlTextWriterStyle.Position);
  RegisterAttribute('text-align', TdxHtmlTextWriterStyle.TextAlign);
  RegisterAttribute('text-decoration', TdxHtmlTextWriterStyle.TextDecoration);
  RegisterAttribute('text-overflow', TdxHtmlTextWriterStyle.TextOverflow);
  RegisterAttribute('top', TdxHtmlTextWriterStyle.Top);
  RegisterAttribute('vertical-align', TdxHtmlTextWriterStyle.VerticalAlign);
  RegisterAttribute('visibility', TdxHtmlTextWriterStyle.Visibility);
  RegisterAttribute('width', TdxHtmlTextWriterStyle.Width);
  RegisterAttribute('white-space', TdxHtmlTextWriterStyle.WhiteSpace);
  RegisterAttribute('z-index', TdxHtmlTextWriterStyle.ZIndex);
end;

class destructor TdxCssTextWriter.Finalize;
begin
  FStyleLookup.Free;
end;

class procedure TdxCssTextWriter.WriteUrlAttribute(AWriter: TTextWriter; const AUrl: string);
var
  S: string;
  ALength: Integer;
begin
  if TdxStringHelper.StartsWith(AUrl, 'url(') then
  begin
    ALength := Length(AUrl) - 4;
    if AUrl[Length(AUrl)] = ')' then
      Dec(ALength);
    S := Trim(TdxStringHelper.Substring(AUrl, 4, ALength));
  end
  else
    S := AUrl;
  AWriter.Write('url(');
  AWriter.Write(TdxHttpUtility.UrlPathEncode(S));
  AWriter.Write(')');
end;

procedure TdxCssTextWriter.Close;
begin
  FWriter.Close;
end;

procedure TdxCssTextWriter.Flush;
begin
  FWriter.Flush;
end;

class function TdxCssTextWriter.GetStyleKey(const AStyleName: string): TdxHtmlTextWriterStyle;
begin
  if (AStyleName <> '') and FStyleLookup.TryGetValue(LowerCase(AStyleName), Result) then
    Exit;
  Result := TdxHtmlTextWriterStyle.Invalid;
end;

class function TdxCssTextWriter.GetStyleName(AStyleKey: TdxHtmlTextWriterStyle): string;
begin
  Result := FAttributeNameLookup[AStyleKey].Name;
end;

class function TdxCssTextWriter.IsStyleEncoded(AStyleKey: TdxHtmlTextWriterStyle): Boolean;
begin
  Result := FAttributeNameLookup[AStyleKey].Encode;
end;

class procedure TdxCssTextWriter.RegisterAttribute(const AName: string; AKey: TdxHtmlTextWriterStyle;
  AEncode: Boolean = False; AIsUrl: Boolean = False);
begin
  if AKey <> TdxHtmlTextWriterStyle.Invalid then
    FStyleLookup.Add(LowerCase(AName), AKey);
  FAttributeNameLookup[AKey] := TAttributeInformation.Create(AName, AEncode, AIsUrl);
end;

procedure TdxCssTextWriter.Write(AValue: Boolean);
begin
  FWriter.Write(AValue);
end;

procedure TdxCssTextWriter.Write(AValue: Char);
begin
  FWriter.Write(AValue);
end;

procedure TdxCssTextWriter.Write(AValue: Integer);
begin
  FWriter.Write(AValue);
end;

procedure TdxCssTextWriter.Write(const ABuffer: TArray<Char>);
begin
  FWriter.Write(ABuffer);
end;

procedure TdxCssTextWriter.Write(AValue: Double);
begin
  FWriter.Write(AValue);
end;

procedure TdxCssTextWriter.Write(AValue: Int64);
begin
  FWriter.Write(AValue);
end;

procedure TdxCssTextWriter.Write(AValue: TObject);
begin
  FWriter.Write(AValue);
end;

procedure TdxCssTextWriter.Write(AValue: Single);
begin
  FWriter.Write(AValue);
end;

procedure TdxCssTextWriter.Write(const S: string);
begin
  FWriter.Write(S);
end;

procedure TdxCssTextWriter.Write(const ABuffer: TArray<Char>; AIndex: Integer; ACount: Integer);
begin
  FWriter.Write(ABuffer, AIndex, ACount);
end;

procedure TdxCssTextWriter.WriteAttribute(const AName: string; const AValue: string);
begin
  WriteAttribute(FWriter, GetStyleKey(AName), AName, AValue);
end;

procedure TdxCssTextWriter.WriteAttribute(AKey: TdxHtmlTextWriterStyle; const AValue: string);
begin
  WriteAttribute(FWriter, AKey, GetStyleName(AKey), AValue);
end;

class procedure TdxCssTextWriter.WriteAttribute(AWriter: TTextWriter; AKey: TdxHtmlTextWriterStyle; const AName, AValue: string);
var
  AIsUrl: Boolean;
begin
  AWriter.Write(AName);
  AWriter.Write(':');
  AIsUrl := FAttributeNameLookup[AKey].IsUrl;
  if not AIsUrl then
    AWriter.Write(AValue)
  else
    WriteUrlAttribute(AWriter, AValue);
  AWriter.Write(';');
end;

class procedure TdxCssTextWriter.WriteAttributes(AWriter: TTextWriter; AStyles: TList<TdxWebRenderStyle>);
var
  I: Integer;
  AStyle: TdxWebRenderStyle;
begin
  for I := 0 to AStyles.Count - 1 do
  begin
    AStyle := AStyles[I];
    WriteAttribute(AWriter, AStyle.Key, AStyle.Name, AStyle.Value);
  end;
end;

procedure TdxCssTextWriter.WriteBeginCssRule(const ASelector: string);
begin
  FWriter.Write(ASelector);
  FWriter.Write(' { ');
end;

procedure TdxCssTextWriter.WriteEndCssRule;
begin
  FWriter.WriteLine(' }');
end;

procedure TdxCssTextWriter.WriteLine;
begin
  FWriter.WriteLine;
end;

procedure TdxCssTextWriter.WriteLine(AValue: Boolean);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(AValue: Char);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(AValue: Double);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(const ABuffer: TArray<Char>);
begin
  FWriter.WriteLine(ABuffer);
end;

procedure TdxCssTextWriter.WriteLine(AValue: Integer);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(AValue: Int64);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(AValue: TObject);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(AValue: Single);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(const S: string);
begin
  FWriter.WriteLine(S);
end;

procedure TdxCssTextWriter.WriteLine(AValue: Cardinal);
begin
  FWriter.WriteLine(AValue);
end;

procedure TdxCssTextWriter.WriteLine(const ABuffer: TArray<Char>; AIndex: Integer; ACount: Integer);
begin
  FWriter.WriteLine(ABuffer, AIndex, ACount);
end;

{ TdxHtmlTextWriter.TTagInformation }

constructor TdxHtmlTextWriter.TTagInformation.Create(const AName: string; ATagType: TTagType;
  const AClosingTag: string);
begin
  Name := AName;
  TagType := ATagType;
  ClosingTag := AClosingTag;
end;

{ TdxHtmlTextWriter.TRenderAttribute }

function TdxHtmlTextWriter.TRenderAttribute.ToString: string;
begin
  Result := Format('RenderAttribute [%s:%s]', [Name, Value.Value]);
end;

{ TdxHtmlTextWriter.TAttributeInformation }

constructor TdxHtmlTextWriter.TAttributeInformation.Create(const AName: string; AEncode, AIsUrl: Boolean);
begin
  Name := AName;
  Encode := AEncode;
  IsUrl := AIsUrl;
end;

{ TdxHtmlTextWriter.TLayout }

constructor TdxHtmlTextWriter.TLayout.Create(AAlignment: TdxWebHorizontalAlign; AWrapping: Boolean);
begin
  Align := AAlignment;
  Wrap := AWrapping;
end;

{ TdxHtmlTextWriter }

constructor TdxHtmlTextWriter.Create(AWriter: TTextWriter; const ATabString: string);
begin
  inherited Create;
  FAttributesList := TList<TRenderAttribute>.Create;
  FCurrentLayout := TLayout.Create(TdxWebHorizontalAlign.NotSet, True);
  FEndTags := TList<TTagStackEntry>.Create;
  FEndTags.Capacity := 16;
  FStyleList := TList<TdxWebRenderStyle>.Create;
  FStyleList.Capacity := 32;
  FWriter := AWriter;
  FTabString := ATabString;
  FIndentLevel := 0;
  FTabsPending := False;
  FIsDescendant := ClassType = TdxHtmlTextWriter;

  FInlineCount := 0;
//  NewLine := #13#10;
end;

constructor TdxHtmlTextWriter.Create(AWriter: TTextWriter);
begin
  Create(AWriter, #9);
end;

destructor TdxHtmlTextWriter.Destroy;
begin
  FStyleList.Free;
  FEndTags.Free;
  FCurrentLayout.Free;
  FAttributesList.Free;
  inherited Destroy;
end;

class constructor TdxHtmlTextWriter.Initialize;
begin
  FTagsLookup := TdxNamedOrdinalDictionary<TdxHtmlTextWriterTag>.Create;
  RegisterTag('', TdxHtmlTextWriterTag.Unknown, TTagType.Other);
  RegisterTag('a', TdxHtmlTextWriterTag.A, TTagType.Inline);
  RegisterTag('acronym', TdxHtmlTextWriterTag.Acronym, TTagType.Inline);
  RegisterTag('address', TdxHtmlTextWriterTag.Address, TTagType.Other);
  RegisterTag('area', TdxHtmlTextWriterTag.Area, TTagType.NonClosing);
  RegisterTag('b', TdxHtmlTextWriterTag.B, TTagType.Inline);
  RegisterTag('base', TdxHtmlTextWriterTag.Base, TTagType.NonClosing);
  RegisterTag('basefont', TdxHtmlTextWriterTag.Basefont, TTagType.NonClosing);
  RegisterTag('bdo', TdxHtmlTextWriterTag.Bdo, TTagType.Inline);
  RegisterTag('bgsound', TdxHtmlTextWriterTag.Bgsound, TTagType.NonClosing);
  RegisterTag('big', TdxHtmlTextWriterTag.Big, TTagType.Inline);
  RegisterTag('blockquote', TdxHtmlTextWriterTag.Blockquote, TTagType.Other);
  RegisterTag('body', TdxHtmlTextWriterTag.Body, TTagType.Other);
  RegisterTag('br', TdxHtmlTextWriterTag.Br, TTagType.Other);
  RegisterTag('button', TdxHtmlTextWriterTag.Button, TTagType.Inline);
  RegisterTag('caption', TdxHtmlTextWriterTag.Caption, TTagType.Other);
  RegisterTag('center', TdxHtmlTextWriterTag.Center, TTagType.Other);
  RegisterTag('cite', TdxHtmlTextWriterTag.Cite, TTagType.Inline);
  RegisterTag('code', TdxHtmlTextWriterTag.Code, TTagType.Inline);
  RegisterTag('col', TdxHtmlTextWriterTag.Col, TTagType.NonClosing);
  RegisterTag('colgroup', TdxHtmlTextWriterTag.Colgroup, TTagType.Other);
  RegisterTag('del', TdxHtmlTextWriterTag.Del, TTagType.Inline);
  RegisterTag('dd', TdxHtmlTextWriterTag.Dd, TTagType.Inline);
  RegisterTag('dfn', TdxHtmlTextWriterTag.Dfn, TTagType.Inline);
  RegisterTag('dir', TdxHtmlTextWriterTag.Dir, TTagType.Other);
  RegisterTag('div', TdxHtmlTextWriterTag.Div, TTagType.Other);
  RegisterTag('dl', TdxHtmlTextWriterTag.Dl, TTagType.Other);
  RegisterTag('dt', TdxHtmlTextWriterTag.Dt, TTagType.Inline);
  RegisterTag('em', TdxHtmlTextWriterTag.Em, TTagType.Inline);
  RegisterTag('embed', TdxHtmlTextWriterTag.Embed, TTagType.NonClosing);
  RegisterTag('fieldset', TdxHtmlTextWriterTag.Fieldset, TTagType.Other);
  RegisterTag('font', TdxHtmlTextWriterTag.Font, TTagType.Inline);
  RegisterTag('form', TdxHtmlTextWriterTag.Form, TTagType.Other);
  RegisterTag('frame', TdxHtmlTextWriterTag.Frame, TTagType.NonClosing);
  RegisterTag('frameset', TdxHtmlTextWriterTag.Frameset, TTagType.Other);
  RegisterTag('h1', TdxHtmlTextWriterTag.H1, TTagType.Other);
  RegisterTag('h2', TdxHtmlTextWriterTag.H2, TTagType.Other);
  RegisterTag('h3', TdxHtmlTextWriterTag.H3, TTagType.Other);
  RegisterTag('h4', TdxHtmlTextWriterTag.H4, TTagType.Other);
  RegisterTag('h5', TdxHtmlTextWriterTag.H5, TTagType.Other);
  RegisterTag('h6', TdxHtmlTextWriterTag.H6, TTagType.Other);
  RegisterTag('head', TdxHtmlTextWriterTag.Head, TTagType.Other);
  RegisterTag('hr', TdxHtmlTextWriterTag.Hr, TTagType.NonClosing);
  RegisterTag('html', TdxHtmlTextWriterTag.Html, TTagType.Other);
  RegisterTag('i', TdxHtmlTextWriterTag.I, TTagType.Inline);
  RegisterTag('iframe', TdxHtmlTextWriterTag.Iframe, TTagType.Other);
  RegisterTag('img', TdxHtmlTextWriterTag.Img, TTagType.NonClosing);
  RegisterTag('input', TdxHtmlTextWriterTag.Input, TTagType.NonClosing);
  RegisterTag('ins', TdxHtmlTextWriterTag.Ins, TTagType.Inline);
  RegisterTag('isindex', TdxHtmlTextWriterTag.Isindex, TTagType.NonClosing);
  RegisterTag('kbd', TdxHtmlTextWriterTag.Kbd, TTagType.Inline);
  RegisterTag('label', TdxHtmlTextWriterTag.Label, TTagType.Inline);
  RegisterTag('legend', TdxHtmlTextWriterTag.Legend, TTagType.Other);
  RegisterTag('li', TdxHtmlTextWriterTag.Li, TTagType.Inline);
  RegisterTag('link', TdxHtmlTextWriterTag.Link, TTagType.NonClosing);
  RegisterTag('map', TdxHtmlTextWriterTag.Map, TTagType.Other);
  RegisterTag('marquee', TdxHtmlTextWriterTag.Marquee, TTagType.Other);
  RegisterTag('menu', TdxHtmlTextWriterTag.Menu, TTagType.Other);
  RegisterTag('meta', TdxHtmlTextWriterTag.Meta, TTagType.NonClosing);
  RegisterTag('nobr', TdxHtmlTextWriterTag.Nobr, TTagType.Inline);
  RegisterTag('noframes', TdxHtmlTextWriterTag.Noframes, TTagType.Other);
  RegisterTag('noscript', TdxHtmlTextWriterTag.Noscript, TTagType.Other);
  RegisterTag('object', TdxHtmlTextWriterTag.Object, TTagType.Other);
  RegisterTag('ol', TdxHtmlTextWriterTag.Ol, TTagType.Other);
  RegisterTag('option', TdxHtmlTextWriterTag.Option, TTagType.Other);
  RegisterTag('p', TdxHtmlTextWriterTag.P, TTagType.Inline);
  RegisterTag('param', TdxHtmlTextWriterTag.Param, TTagType.Other);
  RegisterTag('pre', TdxHtmlTextWriterTag.Pre, TTagType.Other);
  RegisterTag('ruby', TdxHtmlTextWriterTag.Ruby, TTagType.Other);
  RegisterTag('rt', TdxHtmlTextWriterTag.Rt, TTagType.Other);
  RegisterTag('q', TdxHtmlTextWriterTag.Q, TTagType.Inline);
  RegisterTag('s', TdxHtmlTextWriterTag.S, TTagType.Inline);
  RegisterTag('samp', TdxHtmlTextWriterTag.Samp, TTagType.Inline);
  RegisterTag('script', TdxHtmlTextWriterTag.Script, TTagType.Other);
  RegisterTag('select', TdxHtmlTextWriterTag.Select, TTagType.Other);
  RegisterTag('small', TdxHtmlTextWriterTag.Small, TTagType.Other);
  RegisterTag('span', TdxHtmlTextWriterTag.Span, TTagType.Inline);
  RegisterTag('strike', TdxHtmlTextWriterTag.Strike, TTagType.Inline);
  RegisterTag('strong', TdxHtmlTextWriterTag.Strong, TTagType.Inline);
  RegisterTag('style', TdxHtmlTextWriterTag.Style, TTagType.Other);
  RegisterTag('sub', TdxHtmlTextWriterTag.Sub, TTagType.Inline);
  RegisterTag('sup', TdxHtmlTextWriterTag.Sup, TTagType.Inline);
  RegisterTag('table', TdxHtmlTextWriterTag.Table, TTagType.Other);
  RegisterTag('tbody', TdxHtmlTextWriterTag.Tbody, TTagType.Other);
  RegisterTag('td', TdxHtmlTextWriterTag.Td, TTagType.Inline);
  RegisterTag('textarea', TdxHtmlTextWriterTag.Textarea, TTagType.Inline);
  RegisterTag('tfoot', TdxHtmlTextWriterTag.Tfoot, TTagType.Other);
  RegisterTag('th', TdxHtmlTextWriterTag.Th, TTagType.Inline);
  RegisterTag('thead', TdxHtmlTextWriterTag.Thead, TTagType.Other);
  RegisterTag('title', TdxHtmlTextWriterTag.Title, TTagType.Other);
  RegisterTag('tr', TdxHtmlTextWriterTag.Tr, TTagType.Other);
  RegisterTag('tt', TdxHtmlTextWriterTag.Tt, TTagType.Inline);
  RegisterTag('u', TdxHtmlTextWriterTag.U, TTagType.Inline);
  RegisterTag('ul', TdxHtmlTextWriterTag.Ul, TTagType.Other);
  RegisterTag('var', TdxHtmlTextWriterTag.Var, TTagType.Inline);
  RegisterTag('wbr', TdxHtmlTextWriterTag.Wbr, TTagType.NonClosing);
  RegisterTag('xml', TdxHtmlTextWriterTag.Xml, TTagType.Other);

  FAttributeLookup := TdxNamedOrdinalDictionary<TdxHtmlTextWriterAttribute>.Create;
  RegisterAttribute('', TdxHtmlTextWriterAttribute.Invalid, True);
  RegisterAttribute('abbr', TdxHtmlTextWriterAttribute.Abbr, True);
  RegisterAttribute('accesskey', TdxHtmlTextWriterAttribute.Accesskey, True);
  RegisterAttribute('align', TdxHtmlTextWriterAttribute.Align, False);
  RegisterAttribute('alt', TdxHtmlTextWriterAttribute.Alt, True);
  RegisterAttribute('autocomplete', TdxHtmlTextWriterAttribute.AutoComplete, False);
  RegisterAttribute('axis', TdxHtmlTextWriterAttribute.Axis, True);
  RegisterAttribute('background', TdxHtmlTextWriterAttribute.Background, True, True);
  RegisterAttribute('bgcolor', TdxHtmlTextWriterAttribute.Bgcolor, False);
  RegisterAttribute('border', TdxHtmlTextWriterAttribute.Border, False);
  RegisterAttribute('bordercolor', TdxHtmlTextWriterAttribute.Bordercolor, False);
  RegisterAttribute('cellpadding', TdxHtmlTextWriterAttribute.Cellpadding, False);
  RegisterAttribute('cellspacing', TdxHtmlTextWriterAttribute.Cellspacing, False);
  RegisterAttribute('checked', TdxHtmlTextWriterAttribute.Checked, False);
  RegisterAttribute('class', TdxHtmlTextWriterAttribute.Class, True);
  RegisterAttribute('cols', TdxHtmlTextWriterAttribute.Cols, False);
  RegisterAttribute('colspan', TdxHtmlTextWriterAttribute.Colspan, False);
  RegisterAttribute('content', TdxHtmlTextWriterAttribute.Content, True);
  RegisterAttribute('coords', TdxHtmlTextWriterAttribute.Coords, False);
  RegisterAttribute('dir', TdxHtmlTextWriterAttribute.Dir, False);
  RegisterAttribute('disabled', TdxHtmlTextWriterAttribute.Disabled, False);
  RegisterAttribute('for', TdxHtmlTextWriterAttribute.For, False);
  RegisterAttribute('headers', TdxHtmlTextWriterAttribute.Headers, True);
  RegisterAttribute('height', TdxHtmlTextWriterAttribute.Height, False);
  RegisterAttribute('href', TdxHtmlTextWriterAttribute.Href, True, True);
  RegisterAttribute('id', TdxHtmlTextWriterAttribute.Id, False);
  RegisterAttribute('line', TdxHtmlTextWriterAttribute.Line, False);
  RegisterAttribute('longdesc', TdxHtmlTextWriterAttribute.Longdesc, True, True);
  RegisterAttribute('maxlength', TdxHtmlTextWriterAttribute.Maxlength, False);
  RegisterAttribute('multiple', TdxHtmlTextWriterAttribute.Multiple, False);
  RegisterAttribute('Name', TdxHtmlTextWriterAttribute.Name, False);
  RegisterAttribute('nowrap', TdxHtmlTextWriterAttribute.Nowrap, False);
  RegisterAttribute('onclick', TdxHtmlTextWriterAttribute.Onclick, True);
  RegisterAttribute('onchange', TdxHtmlTextWriterAttribute.Onchange, True);
  RegisterAttribute('readonly', TdxHtmlTextWriterAttribute.ReadOnly, False);
  RegisterAttribute('rel', TdxHtmlTextWriterAttribute.Rel, False);
  RegisterAttribute('rows', TdxHtmlTextWriterAttribute.Rows, False);
  RegisterAttribute('rowspan', TdxHtmlTextWriterAttribute.Rowspan, False);
  RegisterAttribute('rules', TdxHtmlTextWriterAttribute.Rules, False);
  RegisterAttribute('scope', TdxHtmlTextWriterAttribute.Scope, False);
  RegisterAttribute('selected', TdxHtmlTextWriterAttribute.Selected, False);
  RegisterAttribute('shape', TdxHtmlTextWriterAttribute.Shape, False);
  RegisterAttribute('size', TdxHtmlTextWriterAttribute.Size, False);
  RegisterAttribute('src', TdxHtmlTextWriterAttribute.Src, False, True);
  RegisterAttribute('style', TdxHtmlTextWriterAttribute.Style, False);
  RegisterAttribute('tabindex', TdxHtmlTextWriterAttribute.Tabindex, False);
  RegisterAttribute('target', TdxHtmlTextWriterAttribute.Target, False);
  RegisterAttribute('title', TdxHtmlTextWriterAttribute.Title, True);
  RegisterAttribute('type', TdxHtmlTextWriterAttribute.Type, False);
  RegisterAttribute('usemap', TdxHtmlTextWriterAttribute.Usemap, False);
  RegisterAttribute('valign', TdxHtmlTextWriterAttribute.Valign, False);
  RegisterAttribute('Value', TdxHtmlTextWriterAttribute.Value, True);
  RegisterAttribute('vcard_name', TdxHtmlTextWriterAttribute.VCardName, False);
  RegisterAttribute('width', TdxHtmlTextWriterAttribute.Width, False);
  RegisterAttribute('wrap', TdxHtmlTextWriterAttribute.Wrap, False);
  RegisterAttribute('_designerRegion', TdxHtmlTextWriterAttribute.DesignerRegion, False);
end;

class destructor TdxHtmlTextWriter.Finalize;
begin
  FTagsLookup.Free;
  FAttributeLookup.Free;
end;

procedure TdxHtmlTextWriter.AddAttribute(const AName: string; const AValue: TdxNullableString);
var
  AAttributeKey: TdxHtmlTextWriterAttribute;
begin
  AAttributeKey := GetAttributeKey(AName);
  AddAttribute(AName, EncodeAttributeValue(AAttributeKey, AValue), AAttributeKey);
end;

procedure TdxHtmlTextWriter.AddAttribute(const AName: string; const AValue: string);
begin
  AddAttribute(AName, TdxNullableString.Create(True, AValue));
end;

procedure TdxHtmlTextWriter.AddAttribute(AKey: TdxHtmlTextWriterAttribute; const AValue: TdxNullableString);
var
  AInformation: TAttributeInformation;
begin
  if AKey <> TdxHtmlTextWriterAttribute.Invalid then
  begin
    AInformation := FAttributeNameLookup[AKey];
    AddAttribute(AInformation.Name, AValue, AKey, AInformation.Encode, AInformation.IsUrl);
  end;
end;

procedure TdxHtmlTextWriter.AddAttribute(AKey: TdxHtmlTextWriterAttribute; const AValue: string);
begin
  AddAttribute(AKey, TdxNullableString.Create(True, AValue));
end;

procedure TdxHtmlTextWriter.AddAttribute(const AName: string; const AValue: TdxNullableString; AEndode: Boolean);
var
  AAttributeKey: TdxHtmlTextWriterAttribute;
begin
  AAttributeKey := GetAttributeKey(AName);
  if AAttributeKey <> TdxHtmlTextWriterAttribute.Invalid then
    AddAttribute(AName, EncodeAttributeValue(AValue, AEndode), AAttributeKey);
end;

procedure TdxHtmlTextWriter.AddAttribute(AKey: TdxHtmlTextWriterAttribute; const AValue: TdxNullableString; AEncode: Boolean);
var
  AInformation: TAttributeInformation;
begin
  if AKey <> TdxHtmlTextWriterAttribute.Invalid then
  begin
    AInformation := FAttributeNameLookup[AKey];
    AddAttribute(AInformation.Name, AValue, AKey, AEncode, AInformation.IsUrl);
  end;
end;

procedure TdxHtmlTextWriter.AddAttribute(const AName: string; const AValue: TdxNullableString; AKey: TdxHtmlTextWriterAttribute; AEncode: Boolean = False; AIsUrl: Boolean = False);
var
  AAttribute, ASearchAttribute: TRenderAttribute;
  AReplaced: Boolean;
  I: Integer;
begin
  AAttribute.Name := AName;
  AAttribute.Value := AValue;
  AAttribute.Key := AKey;
  AAttribute.Encode := AEncode;
  AAttribute.IsUrl := AIsUrl;

  AReplaced := False;
  for I := 0 to FAttributesList.Count - 1 do
  begin
    ASearchAttribute := FAttributesList[I];
    if ASearchAttribute.Name = AName then
    begin
      FAttributesList[I] := AAttribute;
      AReplaced := True;
      Break;
    end;
  end;
  if not AReplaced then
    FAttributesList.Add(AAttribute);
end;

procedure TdxHtmlTextWriter.AddStyleAttribute(const AName: string; const AValue: string);
begin
  AddStyleAttribute(AName, AValue, TdxCssTextWriter.GetStyleKey(AName));
end;

procedure TdxHtmlTextWriter.AddStyleAttribute(AKey: TdxHtmlTextWriterStyle; const AValue: string);
begin
  AddStyleAttribute(TdxCssTextWriter.GetStyleName(AKey), AValue, AKey);
end;

procedure TdxHtmlTextWriter.AddStyleAttribute(const AName: string; const AValue: string; AKey: TdxHtmlTextWriterStyle);
var
  AStyle: TdxWebRenderStyle;
  AStr: string;
begin
  if AValue = '' then
    Exit;
  AStyle.Name := AName;
  AStyle.Key := AKey;
  AStr := AValue;
  if TdxCssTextWriter.IsStyleEncoded(AKey) then
    AStr := TdxHttpUtility.HtmlAttributeEncode(AValue);
  AStyle.Value := AStr;
  FStyleList.Add(AStyle);
end;

procedure TdxHtmlTextWriter.BeginRender;
begin
end;

procedure TdxHtmlTextWriter.Close;
begin
  FWriter.Close;
end;

function TdxHtmlTextWriter.EncodeAttributeValue(const AValue: TdxNullableString; AEncode: Boolean): TdxNullableString;
begin
  if not AValue.HasValue or not AEncode then
    Exit(AValue);
  Result := TdxHttpUtility.HtmlAttributeEncode(AValue.Value);
end;

function TdxHtmlTextWriter.EncodeAttributeValue(AAttrKey: TdxHtmlTextWriterAttribute; const AValue: TdxNullableString): TdxNullableString;
begin
  Result := EncodeAttributeValue(AValue, FAttributeNameLookup[AAttrKey].Encode);
end;

function TdxHtmlTextWriter.EncodeUrl(const AUrl: string): string;
begin
  if not TdxHttpUtility.IsUncSharePath(AUrl) then
    Result := TdxHttpUtility.UrlPathEncode(AUrl)
  else
    Result := AUrl;
end;

procedure TdxHtmlTextWriter.EndRender;
begin
end;

procedure TdxHtmlTextWriter.EnterStyle(AStyle: TdxWebStyle; ATag: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span);
begin
  if not AStyle.IsEmpty or (ATag <> TdxHtmlTextWriterTag.Span) then
  begin
    AStyle.AddAttributesToRender(Self);
    RenderBeginTag(ATag);
  end;
end;

procedure TdxHtmlTextWriter.ExitStyle(AStyle: TdxWebStyle; ATag: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span);
begin
  if not AStyle.IsEmpty or (ATag <> TdxHtmlTextWriterTag.Span) then
    RenderEndTag;
end;

procedure TdxHtmlTextWriter.FilterAttributes;
var
  I, AIndex: Integer;
  AStyle: TdxWebRenderStyle;
  AAttribute: TRenderAttribute;
begin
  AIndex := 0;
  for I := 0 to FStyleList.Count - 1 do
  begin
    AStyle := FStyleList[I];
    if OnStyleAttributeRender(AStyle.Name, AStyle.Value, AStyle.Key) then
    begin
      FStyleList[AIndex] := AStyle;
      Inc(AIndex);
    end;
  end;
  FStyleList.DeleteRange(AIndex, FStyleList.Count - AIndex);

  AIndex := 0;
  for I := 0 to FAttributesList.Count - 1 do
  begin
    AAttribute := FAttributesList[I];
    if OnAttributeRender(AAttribute.Name, AAttribute.Value, AAttribute.Key) then
    begin
      FAttributesList[AIndex] := AAttribute;
      Inc(AIndex);
    end;
  end;
end;

procedure TdxHtmlTextWriter.Flush;
begin
  FWriter.Flush;
end;

function TdxHtmlTextWriter.GetAttributeKey(const AAttrName: string): TdxHtmlTextWriterAttribute;
begin
  if (AAttrName <> '') and FAttributeLookup.TryGetValue(LowerCase(AAttrName), Result) then
    Exit(Result);
  Result := TdxHtmlTextWriterAttribute.Invalid;
end;

function TdxHtmlTextWriter.GetAttributeName(AAttrKey: TdxHtmlTextWriterAttribute): string;
begin
  Result := FAttributeNameLookup[AAttrKey].Name;
end;

function TdxHtmlTextWriter.GetStyleKey(const AStyleName: string): TdxHtmlTextWriterStyle;
begin
  Result := TdxCssTextWriter.GetStyleKey(AStyleName);
end;

function TdxHtmlTextWriter.GetStyleName(AStyleKey: TdxHtmlTextWriterStyle): string;
begin
  Result := TdxCssTextWriter.GetStyleName(AStyleKey);
end;

function TdxHtmlTextWriter.GetTagKey(const ATagName: string): TdxHtmlTextWriterTag;
begin
  if (ATagName <> '') and FTagsLookup.TryGetValue(LowerCase(ATagName), Result) then
    Exit;
  Result := TdxHtmlTextWriterTag.Unknown;
end;

function TdxHtmlTextWriter.GetTagName(ATagKey: TdxHtmlTextWriterTag): string;
begin
  Result := FTagNameLookup[ATagKey].Name;
end;

function TdxHtmlTextWriter.IsAttributeDefined(AKey: TdxHtmlTextWriterAttribute): Boolean;
var
  AAttribute: TRenderAttribute;
begin
  for AAttribute in FAttributesList do
    if AAttribute.Key = AKey then
      Exit(True);
  Result := False;
end;

function TdxHtmlTextWriter.IsAttributeDefined(AKey: TdxHtmlTextWriterAttribute; out AValue: string): Boolean;
var
  AAttribute: TRenderAttribute;
begin
  AValue := '';
  for AAttribute in FAttributesList do
    if AAttribute.Key = AKey then
    begin
      AValue := AAttribute.Value;
      Exit(True);
    end;
  Result := False;
end;

function TdxHtmlTextWriter.IsStyleAttributeDefined(AKey: TdxHtmlTextWriterStyle): Boolean;
var
  AStyle: TdxWebRenderStyle;
begin
  for AStyle in FStyleList do
    if AStyle.Key = AKey then
      Exit(True);
  Result := False;
end;

function TdxHtmlTextWriter.IsStyleAttributeDefined(AKey: TdxHtmlTextWriterStyle; out AValue: string): Boolean;
var
  AStyle: TdxWebRenderStyle;
begin
  AValue := '';
  for AStyle in FStyleList do
    if AStyle.Key = AKey then
    begin
      AValue := AStyle.Value;
      Exit(True);
    end;
  Result := False;
end;

function TdxHtmlTextWriter.IsValidFormAttribute(const AAttribute: string): Boolean;
begin
  Result := True;
end;

function TdxHtmlTextWriter.OnAttributeRender(const AName: string; const AValue: string; AKey: TdxHtmlTextWriterAttribute): Boolean;
begin
  Result := True;
end;

function TdxHtmlTextWriter.OnStyleAttributeRender(const AName: string; const AValue: string; AKey: TdxHtmlTextWriterStyle): Boolean;
begin
  Result := True;
end;

function TdxHtmlTextWriter.OnTagRender(const AName: string; AKey: TdxHtmlTextWriterTag): Boolean;
begin
  Result := True;
end;

procedure TdxHtmlTextWriter.OpenDiv;
begin
  OpenDiv(FCurrentLayout, (FCurrentLayout <> nil) and (FCurrentLayout.Align <> TdxWebHorizontalAlign.NotSet), (FCurrentLayout <> nil) and not FCurrentLayout.Wrap);
end;

procedure TdxHtmlTextWriter.OpenDiv(ALayout: TLayout; AWriteHorizontalAlign: Boolean; AWriteWrapping: Boolean);
var
  AStr: string;
begin
  WriteBeginTag('div');
  if AWriteHorizontalAlign then
  begin
    case ALayout.Align of
      TdxWebHorizontalAlign.Center:
        AStr := 'text-align:center';
      TdxWebHorizontalAlign.Right:
        AStr := 'text-align:right';
      else
        AStr := 'text-align:left';
    end;
    WriteAttribute('style', AStr);
  end;
  if AWriteWrapping then
  begin
    if ALayout.Wrap then
      WriteAttribute('mode', 'wrap')
    else
      WriteAttribute('mode', 'nowrap');
  end;
  Write('>');
  FCurrentWrittenLayout := ALayout;
end;

procedure TdxHtmlTextWriter.OutputTabs;
var
  I: Integer;
begin
  if FTabsPending then
  begin
    for I := 0 to FIndentLevel - 1 do
      FWriter.Write(FTabString);
    FTabsPending := False;
  end;
end;

function TdxHtmlTextWriter.PopEndTag: string;
var
  ATag: TTagStackEntry;
begin
  if FEndTags.Count <= 0 then
    raise EInvalidOperation.Create('HTMLTextWriterUnbalancedPop');

  ATag := FEndTags[FEndTags.Count - 1];
  FEndTags.Delete(FEndTags.Count - 1);
  TagKey := ATag.TagKey;
  Result := ATag.EndTagText;
end;

procedure TdxHtmlTextWriter.PushEndTag(const AEndTag: string);
var
  ATag: TTagStackEntry;
begin
  ATag.TagKey := FTagKey;
  ATag.EndTagText := AEndTag;
  FEndTags.Add(ATag);
end;

class procedure TdxHtmlTextWriter.RegisterAttribute(const AName: string; AKey: TdxHtmlTextWriterAttribute; AEncode: Boolean = False; AIsUrl: Boolean = False);
begin
  if AKey <> TdxHtmlTextWriterAttribute.Invalid then
    FAttributeLookup.Add(LowerCase(AName), AKey);
  FAttributeNameLookup[AKey] := TAttributeInformation.Create(AName, AEncode, AIsUrl);
end;

class procedure TdxHtmlTextWriter.RegisterStyle(const AName: string; AKey: TdxHtmlTextWriterStyle);
begin
  TdxCssTextWriter.RegisterAttribute(AName, AKey);
end;

class procedure TdxHtmlTextWriter.RegisterTag(const AName: string; AKey: TdxHtmlTextWriterTag; AType: TTagType = TTagType.Other);
var
  S: string;
begin
  S := LowerCase(AName);
  FTagsLookup.Add(S, AKey);

  if (AType <> TTagType.NonClosing) and (AKey <> TdxHtmlTextWriterTag.Unknown) then
    S := Format('</%s>', [S])
  else
    S := '';

  FTagNameLookup[AKey] := TTagInformation.Create(AName, AType, S);
end;

function TdxHtmlTextWriter.RenderAfterContent: string;
begin
  Result := '';
end;

function TdxHtmlTextWriter.RenderAfterTag: string;
begin
  Result := '';
end;

function TdxHtmlTextWriter.RenderBeforeContent: string;
begin
  Result := '';
end;

function TdxHtmlTextWriter.RenderBeforeTag: string;
begin
  Result := '';
end;

procedure TdxHtmlTextWriter.RenderBeginTag(const ATagName: string);
begin
  TagName := ATagName;
  RenderBeginTag(FTagKey);
end;

procedure TdxHtmlTextWriter.RenderBeginTag(ATagKey: TdxHtmlTextWriterTag);
var
  AFlag, AFlag2: Boolean;
  AEndTag, AUrl, S: string;
  AInformation: TTagInformation;
  ATagType: TTagType;
  AAttribute: TRenderAttribute;
begin
  TagKey := ATagKey;
  AFlag := TagKey <> TdxHtmlTextWriterTag.Unknown;
  if FIsDescendant then
  begin
    AFlag := OnTagRender(FTagName, FTagKey);
    FilterAttributes;
    S := RenderBeforeTag;
    if S <> '' then
    begin
      if FTabsPending then
        OutputTabs;
      FWriter.Write(S);
    end;
  end;

  AInformation := FTagNameLookup[FTagIndex];
  ATagType := AInformation.TagType;
  AFlag2 := AFlag and (ATagType <> TTagType.NonClosing);
  if AFlag2 then
    AEndTag := AInformation.ClosingTag
  else
    AEndTag := '';
  if AFlag then
  begin
    if FTabsPending then
      OutputTabs;
    FWriter.Write('<');
    FWriter.Write(FTagName);
    S := '';
    for AAttribute in FAttributesList do
    begin
      if AAttribute.Key = TdxHtmlTextWriterAttribute.Style then
        S := AAttribute.Value
      else
      begin
        FWriter.Write(' ');
        FWriter.Write(AAttribute.Name);
        if AAttribute.Value.HasValue then
        begin
          FWriter.Write('="');
          AUrl := AAttribute.Value;
          if AAttribute.IsUrl and ((AAttribute.Key <> TdxHtmlTextWriterAttribute.Href) or not TdxStringHelper.StartsWith(AUrl, 'javascript:')) then
            AUrl := EncodeUrl(AUrl);
          if AAttribute.Encode then
            WriteHtmlAttributeEncode(AUrl)
          else
            FWriter.Write(AUrl);
          FWriter.Write('"');
        end;
      end;
    end;
    if (FStyleList.Count > 0) or (S <> '') then
    begin
      FWriter.Write(' ');
      FWriter.Write('style');
      FWriter.Write('="');
      TdxCssTextWriter.WriteAttributes(FWriter, FStyleList);
      if S <> '' then
        FWriter.Write(S);
      FWriter.Write('"');
    end;
    if ATagType = TTagType.NonClosing then
      FWriter.Write(' />')
    else
      FWriter.Write('>');
  end;

  S := RenderBeforeContent;
  if S <> '' then
  begin
    if FTabsPending then
      OutputTabs;
    FWriter.Write(S);
  end;

  if AFlag2 then
  begin
    if ATagType = TTagType.Inline then
      Inc(FInlineCount)
    else
    begin
      WriteLine;
      Indent := Indent + 1;
    end;
    if AEndTag = '' then
      AEndTag := Format('</%s>', [FTagName]);
  end;

  if FIsDescendant then
  begin
    S := RenderAfterTag;
    if S <> '' then
      if AEndTag = '' then
        AEndTag := S
      else
        AEndTag := S + AEndTag;

    S := RenderAfterContent;
    if S <> '' then
      if AEndTag = '' then
        AEndTag := S
      else
        AEndTag := S + AEndTag;
  end;
  PushEndTag(AEndTag);
  FStyleList.Clear;
  FAttributesList.Clear;
end;

procedure TdxHtmlTextWriter.RenderEndTag;
var
  S: string;
begin
  S := PopEndTag;
  if S <> '' then
  begin
    if FTagNameLookup[FTagIndex].TagType = TTagType.Inline then
    begin
      Dec(FInlineCount);
      Write(S);
    end
    else
    begin
      Indent := Indent - 1;
      Write(S);
      WriteLine;
    end;
  end;
end;

procedure TdxHtmlTextWriter.Write(AValue: Boolean);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(AValue);
end;

procedure TdxHtmlTextWriter.Write(AValue: Char);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(AValue);
end;

procedure TdxHtmlTextWriter.Write(const ABuffer: TArray<Char>);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(ABuffer);
end;

procedure TdxHtmlTextWriter.Write(AValue: Double);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(AValue);
end;

procedure TdxHtmlTextWriter.Write(AValue: Integer);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(AValue);
end;

procedure TdxHtmlTextWriter.Write(AValue: Int64);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(AValue);
end;

procedure TdxHtmlTextWriter.Write(AValue: TObject);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(AValue);
end;

procedure TdxHtmlTextWriter.Write(AValue: Single);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(AValue);
end;

procedure TdxHtmlTextWriter.Write(const S: string);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(S);
end;

procedure TdxHtmlTextWriter.Write(const ABuffer: TArray<Char>; AIndex, ACount: Integer);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write(ABuffer, AIndex, ACount);
end;

procedure TdxHtmlTextWriter.WriteAttribute(const AName, AValue: string; AEncode: Boolean = False);
begin
  FWriter.Write(' ');
  FWriter.Write(AName);
  if AValue <> '' then
  begin
    FWriter.Write('="');
    if AEncode then
      WriteHtmlAttributeEncode(AValue)
    else
      FWriter.Write(AValue);
    FWriter.Write('"');
  end;
end;

procedure TdxHtmlTextWriter.WriteBeginTag(const ATagName: string);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write('<');
  FWriter.Write(ATagName);
end;

procedure TdxHtmlTextWriter.WriteBreak;
begin
  Write('<br />');
end;

procedure TdxHtmlTextWriter.WriteEncodedText(const AText: string);
var
  ALength, AStartIndex, AIndex: Integer;
begin
  if AText = '' then
    raise EArgumentNilException.Create('text');
  ALength := Length(AText);
  AStartIndex := 0;
  while AStartIndex < ALength do
  begin
    AIndex := TdxStringHelper.IndexOf(AText, #$00A0, AStartIndex);
    if AIndex < 0 then
    begin
      if AStartIndex = 0 then
        TdxHttpUtility.HtmlEncode(AText, Self)
      else
        TdxHttpUtility.HtmlEncode(TdxStringHelper.Substring(AText, AStartIndex, ALength - AStartIndex), Self);
      AStartIndex := ALength;
    end
    else
    begin
      if AIndex > AStartIndex then
        TdxHttpUtility.HtmlEncode(TdxStringHelper.Substring(AText, AStartIndex, AIndex - AStartIndex), Self);
      Write('&nbsp;');
      AStartIndex := AIndex + 1;
    end;
  end;
end;

procedure TdxHtmlTextWriter.WriteEncodedUrl(const AUrl: string);
var
  AIndex: Integer;
begin
  AIndex := TdxStringHelper.IndexOf(AUrl, '?');
  if AIndex <> -1 then
  begin
    WriteUrlEncodedString(TdxStringHelper.Substring(AUrl, 0, AIndex), False);
    Write(TdxStringHelper.Substring(AUrl, AIndex));
  end
  else
    WriteUrlEncodedString(AUrl, False);
end;

procedure TdxHtmlTextWriter.WriteEncodedUrlParameter(const AUrlText: string);
begin
  WriteUrlEncodedString(AUrlText, True);
end;

procedure TdxHtmlTextWriter.WriteEndTag(const ATagName: string);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write('<');
  FWriter.Write('/');
  FWriter.Write(ATagName);

  FWriter.Write('>');
end;

procedure TdxHtmlTextWriter.WriteFullBeginTag(const ATagName: string);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.Write('<');
  FWriter.Write(ATagName);
  FWriter.Write('>');
end;

procedure TdxHtmlTextWriter.WriteHtmlAttributeEncode(const S: string);
begin
  TdxHttpUtility.HtmlAttributeEncode(S, FWriter);
end;

procedure TdxHtmlTextWriter.WriteLine;
begin
  FWriter.WriteLine;
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(AValue: Boolean);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(AValue);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(AValue: Char);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(AValue);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(AValue: Integer);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(AValue);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(const ABuffer: TArray<Char>);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(ABuffer);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(AValue: Double);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(AValue);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(AValue: Int64);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(AValue);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(AValue: TObject);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(AValue);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(AValue: Single);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(AValue);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(const S: string);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(S);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLine(const ABuffer: TArray<Char>; AIndex, ACount: Integer);
begin
  if FTabsPending then
    OutputTabs;
  FWriter.WriteLine(ABuffer, AIndex, ACount);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteLineNoTabs(const S: string);
begin
  FWriter.WriteLine(S);
  FTabsPending := True;
end;

procedure TdxHtmlTextWriter.WriteObsoleteBreak;
begin
  Write('<br>');
end;

procedure TdxHtmlTextWriter.WriteStyleAttribute(const AName, AValue: string; AEncode: Boolean = False);
begin
  FWriter.Write(AName);
  FWriter.Write(':');
  if AEncode then
    WriteHtmlAttributeEncode(AValue)
  else
    FWriter.Write(AValue);
  FWriter.Write(';');
end;

procedure TdxHtmlTextWriter.WriteUrlEncodedString(const AText: string; AArgument: Boolean);
var
  I: Integer;
  C: Char;
begin
  for I := 1 to Length(AText) do
  begin
    C := AText[I];
    if TdxHttpUtility.IsSafe(C) then
      Write(C)
    else
      if not AArgument and ((C = '/') or (C = ':') or (C = '#') or (C = ',')) then
        Write(C)
      else
        if (C = ' ') and AArgument then
          Write('+')
        else
          if (Ord(C) and $ff80) = 0 then
          begin
            Write('%');
            Write(TdxHttpUtility.IntToHex((Ord(C) shr 4) and $F));
            Write(TdxHttpUtility.IntToHex(Ord(C) and $F));
          end
          else
            Write(TdxHttpUtility.UrlEncodeNonAscii(C, TdxEncoding.UTF8));
  end;
end;

procedure TdxHtmlTextWriter.SetIndent(const AValue: Integer);
begin
  if AValue < 0 then
    FIndentLevel := 0
  else
    FIndentLevel := AValue;
end;

function TdxHtmlTextWriter.GetRenderDivAroundHiddenInputs: Boolean;
begin
  Result := True;
end;

procedure TdxHtmlTextWriter.SetTagKey(const AValue: TdxHtmlTextWriterTag);
begin
  FTagIndex := AValue;
  FTagKey := AValue;
  if AValue <> TdxHtmlTextWriterTag.Unknown then
    FTagName := FTagNameLookup[FTagIndex].Name;
end;

procedure TdxHtmlTextWriter.SetTagName(const AValue: string);
begin
  FTagName := AValue;
  FTagKey := GetTagKey(FTagName);
  FTagIndex := FTagKey;
end;

end.
