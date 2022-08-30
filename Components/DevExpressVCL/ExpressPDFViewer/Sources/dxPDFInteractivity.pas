{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFInteractivity;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Graphics, dxCoreClasses, dxCoreGraphics,
  cxGeometry, dxPDFBase, dxPDFTypes, dxPDFCore, dxPDFRecognizedObject;

const
  dxPDFOutlineTreeItemDefaultColor = clNone;

type
  TdxPDFOutline = class;

  { TdxPDFFitDestination }

  TdxPDFFitDestination = class(TdxPDFCustomDestination)
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;
  end;

  { TdxPDFFitRectangleDestination }

  TdxPDFFitRectangleDestination = class(TdxPDFCustomDestination)
  strict private
    FRectangle: TdxRectF;
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;

    property Rectangle: TdxRectF read FRectangle;
  end;

  { TdxPDFFitHorizontallyDestination }

  TdxPDFFitHorizontallyDestination = class(TdxPDFCustomDestination)
  strict private
    FTop: Single;
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;

    property Top: Single read FTop;
  end;

  { TdxPDFFitVerticallyDestination }

  TdxPDFFitVerticallyDestination = class(TdxPDFCustomDestination)
  strict private
    FLeft: Single;
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;

    property Left: Single read FLeft;
  end;

  { TdxPDFXYZDestination }

  TdxPDFXYZDestination = class(TdxPDFCustomDestination)
  strict private
    FLeft: Single;
    FTop: Single;
    FZoom: Single;
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;

    property Left: Single read FLeft;
    property Top: Single read FTop;
    property Zoom: Single read FZoom;
  end;

  { TdxPDFFitBBoxDestination }

  TdxPDFFitBBoxDestination = class(TdxPDFCustomDestination)
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;
  end;

  { TdxPDFFitBBoxHorizontallyDestination }

  TdxPDFFitBBoxHorizontallyDestination = class(TdxPDFCustomDestination)
  strict private
    FTop: Single;
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;

    property Top: Single read FTop;
  end;

  { TdxPDFFitBBoxVerticallyDestination }

  TdxPDFFitBBoxVerticallyDestination = class(TdxPDFCustomDestination)
  strict private
    FLeft: Single;
  protected
    class function GetTypeName: string; override;
    function GetTarget: TdxPDFTarget; override;
    procedure ReadParameters(AArray: TdxPDFArray); override;

    property Left: Single read FLeft;
  end;

  { TdxPDFOutlineItem }

  TdxPDFOutlineItem = class(TdxPDFObject)
  strict private
    FClosed: Boolean;
    FCount: Integer;
    FFirst: TdxPDFOutline;
    FLast: TdxPDFOutline;
  protected
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    function UpdateCount: Integer;

    property Closed: Boolean read FClosed write FClosed;
    property Count: Integer read FCount;
    property First: TdxPDFOutline read FFirst write FFirst;
    property Last: TdxPDFOutline read FLast write FLast;
  end;

  { TdxPDFOutline }

  TdxPDFOutline = class(TdxPDFOutlineItem)
  strict private
    FAction: TdxPDFCustomAction;
    FCatalog: TdxPDFCatalog;
    FColor: TdxPDFColor;
    FDestinationInfo: TdxPDFDestinationInfo;
    FIsBold: Boolean;
    FIsItalic: Boolean;
    FNext: TdxPDFOutline;
    FParent: TdxPDFOutlineItem;
    FPrev: TdxPDFOutline;
    FTitle: string;
    function GetActualDestination: TdxPDFCustomDestination;
    function GetDestination: TdxPDFCustomDestination;
    procedure SetAction(const AValue: TdxPDFCustomAction);
    procedure SetNext(const AValue: TdxPDFOutline);
    procedure ReadColor(ADictionary: TdxPDFDictionary);
    procedure ReadFlags(ADictionary: TdxPDFDictionary);
  protected
    class function GetTypeName: string; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      AParent: TdxPDFOutlineItem; APrev: TdxPDFOutline); reintroduce; overload;

    property Action: TdxPDFCustomAction read FAction write SetAction;
    property ActualDestination: TdxPDFCustomDestination read GetActualDestination;
    property Color: TdxPDFColor read FColor;
    property Destination: TdxPDFCustomDestination read GetDestination;
    property DestinationInfo: TdxPDFDestinationInfo read FDestinationInfo;
    property IsBold: Boolean read FIsBold;
    property IsItalic: Boolean read FIsItalic;
    property Next: TdxPDFOutline read FNext write SetNext;
    property Parent: TdxPDFOutlineItem read FParent;
    property Prev: TdxPDFOutline read FPrev write FPrev;
    property Title: string read FTitle;
  end;

  { TdxPDFOutlines }

  TdxPDFOutlines = class(TdxPDFOutlineItem)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFOutlineTreeItem }

  TdxPDFOutlineTreeItem = class
  strict private
    FColor: TColor;
    FHasChildren: Boolean;
    FID: Integer;
    FInteractiveOperation: TdxPDFInteractiveOperation;
    FIsBold: Boolean;
    FIsItalic: Boolean;
    FOutline: TdxPDFOutline;
    FParentID: Integer;
    FTitle: string;
    function GetInteractiveOperation: TdxPDFInteractiveOperation;
  protected
    procedure Read(AOutline: TdxPDFOutline; AID, AParentID: Integer);
    property Outline: TdxPDFOutline read FOutline;

    property Color: TColor read FColor;
    property HasChildren: Boolean read FHasChildren;
    property ID: Integer read FID;
    property InteractiveOperation: TdxPDFInteractiveOperation read GetInteractiveOperation;
    property IsBold: Boolean read FIsBold;
    property IsItalic: Boolean read FIsItalic;
    property ParentID: Integer read FParentID;
    property Title: string read FTitle;
  end;

  { TdxPDFOutlineTree }

  TdxPDFOutlineTree = class(TObjectList<TdxPDFOutlineTreeItem>)
  strict private
    FPages: TdxPDFPages;
    function AddOutline(AOutline: TdxPDFOutline; AParentID: Integer): Integer;
    function GetNextPageNumber(AOutline: TdxPDFOutline): Integer;
    function GetPageNumber(AOutline: TdxPDFOutline): Integer;
  protected
    function GetPrintPageNumbers(AItems: TList<TdxPDFOutlineTreeItem>; APrintSection: Boolean): TIntegerDynArray;
    procedure Read(ACatalog: TdxPDFCatalog);
  end;

  { TdxPDFJumpAction }

  TdxPDFJumpAction = class(TdxPDFCustomAction)
  strict private
    FDestination: TdxPDFCustomDestination;
    FDestinationInfo: TdxPDFDestinationInfo;
    function GetDestination: TdxPDFCustomDestination;
    procedure SetDestination(const AValue: TdxPDFCustomDestination);
  protected
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    function IsInternal: Boolean; virtual; abstract;

    property Destination: TdxPDFCustomDestination read GetDestination write SetDestination;
  end;

  { TdxPDFGoToAction }

  TdxPDFGoToAction = class(TdxPDFJumpAction)
  protected
    class function GetTypeName: string; override;
    function IsInternal: Boolean; override;
    procedure Execute(const AController: IdxPDFInteractivityController); override;
  end;

  { TdxPDFURIAction }

  TdxPDFURIAction = class(TdxPDFCustomAction)
  strict private
    FURI: string;
  protected
    class function GetTypeName: string; override;
    procedure Execute(const AController: IdxPDFInteractivityController); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    property URI: string read FURI;
  end;

  { TdxPDFNamedAction }

  TdxPDFNamedAction = class(TdxPDFCustomAction)
  strict private
    FActionName: string;
  protected
    class function GetTypeName: string; override;
    procedure Execute(const AController: IdxPDFInteractivityController); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    property ActionName: string read FActionName;
  end;


  { TdxPDFEmbeddedGoToAction }

  TdxPDFEmbeddedGoToAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFGoTo3dViewAction }

  TdxPDFGoTo3dViewAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFHideAction }

  TdxPDFHideAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFImportDataAction }

  TdxPDFImportDataAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFJavaScriptAction }

  TdxPDFJavaScriptAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFLaunchAction }

  TdxPDFLaunchAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFMovieAction }

  TdxPDFMovieAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFRemoteGoToAction }

  TdxPDFRemoteGoToAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFRenditionAction }

  TdxPDFRenditionAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFResetFormAction }

  TdxPDFResetFormAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFSetOCGStateAction }

  TdxPDFSetOCGStateAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFSoundAction }

  TdxPDFSoundAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFSubmitFormAction }

  TdxPDFSubmitFormAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFThreadAction }

  TdxPDFThreadAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFTransitionAction }

  TdxPDFTransitionAction = class(TdxPDFCustomAction)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFWidgetAppearanceCharacteristics }

  TdxPDFWidgetAppearanceCharacteristics = class(TdxPDFObject)
  strict private
    FBackgroundColor: TdxPDFColor;
    FBorderColor: TdxPDFColor;
    FCaption: string;
    FRotationAngle: Integer;
  protected
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
  public
    property Caption: string read FCaption;
    property BackgroundColor: TdxPDFColor read FBackgroundColor;
    property BorderColor: TdxPDFColor read FBorderColor;
    property RotationAngle: Integer read FRotationAngle;
  end;

  { TdxPDFWidgetAnnotation }

  TdxPDFActionAnnotation = class(TdxPDFCustomAnnotation)
  strict private
    FAction: TdxPDFCustomAction;
    FDestinationInfo: TdxPDFDestinationInfo;
    FInteractiveOperation: TdxPDFInteractiveOperation;
    function GetDestination: TdxPDFCustomDestination;
    function GetInteractiveOperation: TdxPDFInteractiveOperation;
    procedure SetAction(const AValue: TdxPDFCustomAction);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Resolve(ADictionary: TdxPDFReaderDictionary); override;
  public
    property Action: TdxPDFCustomAction read FAction write SetAction;
    property InteractiveOperation: TdxPDFInteractiveOperation read GetInteractiveOperation;
  end;

  { TdxPDFWidgetAnnotation }

  TdxPDFWidgetAnnotation = class(TdxPDFActionAnnotation)
  strict private
    FAppearanceCharacteristics: TdxPDFWidgetAppearanceCharacteristics;
    FBorderStyle: TdxPDFAnnotationBorderStyle;
    FHighlightingMode: TdxPDFAnnotationHighlightingMode;
    FInteractiveFormField: TdxPDFInteractiveFormField;
    function GetAppearanceContentRectangle: TdxRectF;
    function GetBackgroundColor: TdxPDFColor;
    function GetBorderWidth: Single;

    procedure SetAppearanceCharacteristics(const AValue: TdxPDFWidgetAppearanceCharacteristics);
    procedure SetBorderStyle(const AValue: TdxPDFAnnotationBorderStyle);
    procedure SetInteractiveFormField(const AValue: TdxPDFInteractiveFormField);

    function CreateFormTransformationMatrix: TdxPdfTransformationMatrix;
    procedure ReadAppearanceCharacteristics;
  protected
    class function GetTypeName: string; override;
    function CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField; override;
    function CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject; override;
    function CreateAppearanceForm(const AName: string): TdxPDFForm; override;
    function GetUseDefaultForm: Boolean; override;
    function NeedCreateAppearance(AForm: TdxPDFForm): Boolean; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary; APage: TdxPDFPage); override;

    property BorderWidth: Single read GetBorderWidth;
  public
    property AppearanceContentRectangle: TdxRectF read GetAppearanceContentRectangle;
    property AppearanceCharacteristics: TdxPDFWidgetAppearanceCharacteristics read FAppearanceCharacteristics write
      SetAppearanceCharacteristics;
    property BackgroundColor: TdxPDFColor read GetBackgroundColor;
    property BorderStyle: TdxPDFAnnotationBorderStyle read FBorderStyle write SetBorderStyle;
    property InteractiveFormField: TdxPDFInteractiveFormField read FInteractiveFormField write SetInteractiveFormField;
    property HighlightingMode: TdxPDFAnnotationHighlightingMode read FHighlightingMode;
  end;

  { TdxPDFLinkAnnotation }

  TdxPDFLinkAnnotation = class(TdxPDFActionAnnotation)
  strict private
    FCatalog: TdxPDFCatalog;
    function GetBounds: TdxRectF;
    function GetHint: string;
  protected
    class function GetTypeName: string; override;
    function CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField; override;
    procedure Resolve(ADictionary: TdxPDFReaderDictionary); override;
  public
    property Bounds: TdxRectF read GetBounds;
    property Hint: string read GetHint;
  end;

  { TdxPDFMarkupAnnotation }

  TdxPDFMarkupAnnotation = class(TdxPDFActionAnnotation)
  strict private
    FOpacity: Single;
    FSubject: string;
    FTitle: string;
    function GetBounds: TdxRectF;
    function GetHint: string;
    function GetTitle: string;
  protected
    function CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField; override;
    procedure Resolve(ADictionary: TdxPDFReaderDictionary); override;

    function GetAnnotationFieldClass: TdxPDFAnnotationFieldClass; virtual;

    property Title: string read GetTitle;
  public
    property Bounds: TdxRectF read GetBounds;
    property Hint: string read GetHint;
    property Opacity: Single read FOpacity;
  end;

  { TdxPDFFileAttachmentAnnotation }

  TdxPDFFileAttachmentAnnotation = class(TdxPDFMarkupAnnotation)
  strict private
    FFileSpecification: TdxPDFFileSpecification;
    FIconName: string;
    function GetFileSpecification: TdxPDFFileSpecification;
    procedure SetFileSpecification(const AValue: TdxPDFFileSpecification);
    procedure InternalSetFileSpecification(const AValue: TdxPDFFileSpecification);
  protected
    class function GetTypeName: string; override;
    function GetAnnotationFieldClass: TdxPDFAnnotationFieldClass; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Resolve(ADictionary: TdxPDFReaderDictionary); override;
  public
    property FileSpecification: TdxPDFFileSpecification read GetFileSpecification write SetFileSpecification;
  end;

  { TdxPDFTextFormField }

  TdxPDFTextFormField = class(TdxPDFInteractiveFormField)
  strict private
    FDefaultText: string;
    FMaxLen: Integer;
    FText: string;
    FValuesProvider: TdxPDFTextFormField;
    function GetDefaultText: string;
    function GetMaxLen: Integer;
    function GetText: string; overload;
    function GetText(ADictionary: TdxPDFReaderDictionary; const AKey: string): string; overload;
  protected
    class function GetTypeName: string; override;
    function CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject; override;
    function GetAcroFieldClass: TdxPDFAcroFormFieldClass; override;
    procedure Read(AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField;
      ADictionary: TdxPDFReaderDictionary; ANumber: Integer); override;
    procedure InternalSetValue(const AValue: Variant; AState: TdxPDFDocumentState); override;
  public
    function GetValue: Variant; override;

    property DefaultText: string read GetDefaultText;
    property MaxLen: Integer read GetMaxLen;
    property Text: string read GetText;
  end;

  { TdxPDFSignatureFormField }

  TdxPDFSignatureFormField = class(TdxPDFInteractiveFormField)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFChoiceField }

  TdxPDFChoiceField = class(TdxPDFInteractiveFormField)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFButtonField }

  TdxPDFButtonField = class(TdxPDFInteractiveFormField)
  protected
    class function GetTypeName: string; override;
    function GetAcroFieldClass: TdxPDFAcroFormFieldClass; override;
  public
    function UseDefaultAppearanceForm: Boolean; override;
  end;

  { TdxPDFAcroFormVisualField }

  TdxPDFAcroFormVisualField = class(TdxPDFAcroFormField)
  strict private
    FPrint: Boolean;
    FReadOnly: Boolean;
    FRequired: Boolean;
    FVisible: Boolean;
  protected
    function GetFlags: TdxPDFInteractiveFormFieldFlags; override;

    function GetAnnotationFlags: TdxPDFAnnotationFlags; virtual;

    property AnnotationFlags: TdxPDFAnnotationFlags read GetAnnotationFlags;
    property Print: Boolean read FPrint write FPrint;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Required: Boolean read FRequired write FRequired;
    property Visible: Boolean read FVisible write FVisible;
  public
    constructor Create; override;
  end;

  { TdxPDFAcroFormTextBoxField }

  TdxPDFAcroFormTextBoxField = class(TdxPDFAcroFormVisualField)
  strict private
    FMaxLength: Integer;
    function GetText: string;
    procedure SetMaxLength(const AValue: Integer);
    procedure SetText(const AValue: string);
  protected
    function GetHint: string; override;
    function GetHitCode: Integer; override;

    property MaxLength: Integer read FMaxLength write SetMaxLength;
  public
    property Text: string read GetText write SetText;
  end;

  { TdxPDFAcroFormButton }

  TdxPDFAcroFormButton = class(TdxPDFAcroFormActionField, IdxPDFHintableObject)
  protected
    function GetHint: string; override;
    function GetHitCode: Integer; override;
  end;

implementation

uses
  Math, dxTypeHelpers, dxStringHelper, dxCore, dxPDFCommandConstructor, dxPDFUtils;

type
  TdxPDFAnnotationFieldAccess = class(TdxPDFAnnotationField);
  TdxPDFCatalogAccess = class(TdxPDFCatalog);
  TdxPDFCustomDestinationAccess = class(TdxPDFCustomDestination);
  TdxPDFInteractiveFormCustomFieldAccess = class(TdxPDFInteractiveFormField);

{ TdxPDFFitDestination }

class function TdxPDFFitDestination.GetTypeName: string;
begin
  Result := 'Fit';
end;

function TdxPDFFitDestination.GetTarget: TdxPDFTarget;
begin
  Result := TdxPDFTarget.Create(tmFit, CalculatePageIndex(Pages));
end;

procedure TdxPDFFitDestination.ReadParameters(AArray: TdxPDFArray);
begin
  if AArray.Count >= 2 then
    inherited ReadParameters(AArray);
end;

{ TdxPDFFitRectangleDestination }

class function TdxPDFFitRectangleDestination.GetTypeName: string;
begin
  Result := 'FitR';
end;

function TdxPDFFitRectangleDestination.GetTarget: TdxPDFTarget;
begin
  Result := TdxPDFTarget.Create(tmFitRectangle, CalculatePageIndex(Pages), FRectangle);
end;

procedure TdxPDFFitRectangleDestination.ReadParameters(AArray: TdxPDFArray);
var
  ATemp: Single;
begin
  inherited ReadParameters(AArray);
  if AArray.Count >= 6 then
  begin
    FRectangle.Left := TdxPDFUtils.ConvertToDouble(AArray[2]);
    FRectangle.Bottom := TdxPDFUtils.ConvertToDouble(AArray[3]);
    FRectangle.Right := TdxPDFUtils.ConvertToDouble(AArray[4]);
    FRectangle.Top := TdxPDFUtils.ConvertToDouble(AArray[5]);
    if FRectangle.Right < FRectangle.Left then
    begin
      ATemp := FRectangle.Right;
      FRectangle.Right := FRectangle.Left;
      FRectangle.Left := ATemp;
    end;
    if FRectangle.Top < FRectangle.Bottom then
    begin
      ATemp := FRectangle.Bottom;
      FRectangle.Bottom := FRectangle.Top;
      FRectangle.Top := ATemp;
    end;
  end;
end;
{ TdxPDFFitHorizontallyDestination }

class function TdxPDFFitHorizontallyDestination.GetTypeName: string;
begin
  Result := 'FitH';
end;

function TdxPDFFitHorizontallyDestination.GetTarget: TdxPDFTarget;
begin
  Result := TdxPDFTarget.Create(tmFitHorizontally, CalculatePageIndex(Pages), dxPDFInvalidValue,
    ValidateVerticalCoordinate(FTop));
end;

procedure TdxPDFFitHorizontallyDestination.ReadParameters(AArray: TdxPDFArray);
begin
  inherited ReadParameters(AArray);
  FTop := GetSingleValue(AArray);
end;

{ TdxPDFFitVerticallyDestination }

class function TdxPDFFitVerticallyDestination.GetTypeName: string;
begin
  Result := 'FitV';
end;

function TdxPDFFitVerticallyDestination.GetTarget: TdxPDFTarget;
begin
  Result := TdxPDFTarget.Create(tmFitVertically, CalculatePageIndex(Pages), FLeft, dxPDFInvalidValue);
end;

procedure TdxPDFFitVerticallyDestination.ReadParameters(AArray: TdxPDFArray);
begin
  inherited ReadParameters(AArray);
  FLeft := GetSingleValue(AArray);
end;

{ TdxPDFXYZDestination }

class function TdxPDFXYZDestination.GetTypeName: string;
begin
  Result := TdxPDFKeywords.XYZDestination;
end;

function TdxPDFXYZDestination.GetTarget: TdxPDFTarget;
var
  AActualZoom: Single;
  AValue: Single;
begin
  AActualZoom := dxPDFInvalidValue;
  if TdxPDFUtils.IsDoubleValid(Zoom) then
  begin
    AValue := Zoom;
    if not SameValue(AValue, 0) then
      AActualZoom := AValue;
  end;
  Result := TdxPDFTarget.Create(CalculatePageIndex(Pages), Left, ValidateVerticalCoordinate(Top), AActualZoom);
end;

procedure TdxPDFXYZDestination.ReadParameters(AArray: TdxPDFArray);
begin
  inherited ReadParameters(AArray);
  FZoom := dxPDFInvalidValue;
  case AArray.Count of
    1, 2:
      begin
        FLeft := dxPDFInvalidValue;
        FTop := dxPDFInvalidValue;
      end;
    3:
      begin
        FLeft := dxPDFInvalidValue;
        FTop := TdxPDFUtils.ConvertToSingle(AArray[2]);
      end;
    4:
      begin
        FLeft := TdxPDFUtils.ConvertToSingle(AArray[2]);
        FTop := TdxPDFUtils.ConvertToSingle(AArray[3]);
      end;
  else
    FLeft := TdxPDFUtils.ConvertToSingle(AArray[2]);
    FTop := TdxPDFUtils.ConvertToSingle(AArray[3]);
    FZoom := TdxPDFUtils.ConvertToSingle(AArray[4]);
  end;
end;

{ TdxPDFFitBBoxDestination }

class function TdxPDFFitBBoxDestination.GetTypeName: string;
begin
  Result := 'FitB';
end;

function TdxPDFFitBBoxDestination.GetTarget: TdxPDFTarget;
begin
  Result := TdxPDFTarget.Create(tmFitBBox, CalculatePageIndex(Pages));
end;

procedure TdxPDFFitBBoxDestination.ReadParameters(AArray: TdxPDFArray);
begin
  if AArray.Count >= 2 then
    inherited ReadParameters(AArray);
end;

{ TdxPDFFitBBoxHorizontallyDestination }

class function TdxPDFFitBBoxHorizontallyDestination.GetTypeName: string;
begin
  Result := 'FitBH';
end;

function TdxPDFFitBBoxHorizontallyDestination.GetTarget: TdxPDFTarget;
begin
  Result := TdxPDFTarget.Create(tmFitBBoxHorizontally, CalculatePageIndex(Pages),
    dxPDFInvalidValue, ValidateVerticalCoordinate(FTop));
end;

procedure TdxPDFFitBBoxHorizontallyDestination.ReadParameters(AArray: TdxPDFArray);
begin
  inherited ReadParameters(AArray);
  FTop := GetSingleValue(AArray);
end;

{ TdxPDFFitBBoxVerticallyDestination }

class function TdxPDFFitBBoxVerticallyDestination.GetTypeName: string;
begin
  Result := 'FitBV';
end;

function TdxPDFFitBBoxVerticallyDestination.GetTarget: TdxPDFTarget;
begin
  Result := TdxPDFTarget.Create(tmFitBBoxVertically, CalculatePageIndex(Pages), FLeft, dxPDFInvalidValue);
end;

procedure TdxPDFFitBBoxVerticallyDestination.ReadParameters(AArray: TdxPDFArray);
begin
  inherited ReadParameters(AArray);
  FLeft := GetSingleValue(AArray);
end;

{ TdxPDFOutlineItem }

function TdxPDFOutlineItem.UpdateCount: Integer;
var
  AOutline: TdxPDFOutline;
begin
  FCount := 0;
  AOutline := FFirst;
  while AOutline <> nil do
  begin
    Inc(FCount);
    if not AOutline.Closed then
      FCount := FCount + AOutline.UpdateCount;
    AOutline := AOutline.Next;
  end;
  Result := FCount;
end;

procedure TdxPDFOutlineItem.DestroySubClasses;
begin
  FreeAndNil(FFirst);
  inherited DestroySubClasses;
end;

procedure TdxPDFOutlineItem.Read(ADictionary: TdxPDFReaderDictionary);
var
  ANext: TdxPDFOutline;
  AOutlineObject: TdxPDFBase;
  AOutlineDictionary, ANextDictionary: TdxPDFReaderDictionary;
begin
  inherited Read(ADictionary);
  if (ADictionary <> nil) and ADictionary.TryGetObject(TdxPDFKeywords.OutlineFirst, AOutlineObject) then
  begin
    AOutlineDictionary := Repository.GetDictionary(AOutlineObject.Number);
    if (AOutlineDictionary <> nil) and (AOutlineDictionary <> ADictionary) then
    begin
      FFirst := TdxPDFOutline.Create(Self);
      FFirst.Read(AOutlineDictionary, Self, nil);
      FLast := FFirst;
      ANextDictionary := AOutlineDictionary.GetDictionary(TdxPDFKeywords.OutlineNext);
      while ANextDictionary <> nil do
      begin
        ANext := TdxPDFOutline.Create(Self);
        ANext.Read(ANextDictionary, Self, FLast);
        FLast.Next := ANext;
        FLast := ANext;
        ANextDictionary := ANextDictionary.GetDictionary(TdxPDFKeywords.OutlineNext);
      end;
      FCount := ADictionary.GetInteger(TdxPDFKeywords.OutlineCount, 0);
      FClosed := FCount <= 0;
      UpdateCount;
    end;
  end;
end;

{ TdxPDFOutline }

function TdxPDFOutline.GetActualDestination: TdxPDFCustomDestination;
var
  ADestination: TdxPDFCustomDestination;
begin
  ADestination := Destination;
  if ADestination <> nil then
    Result := ADestination
  else
  begin
    if FAction is TdxPDFGoToAction then
      Result := TdxPDFGoToAction(FAction).Destination
    else
      Result := nil
  end;
end;

function TdxPDFOutline.GetDestination: TdxPDFCustomDestination;
begin
  if not DestinationInfo.IsValid then
    Result := nil
  else
    Result := DestinationInfo.GetDestination(FCatalog, True);
end;

procedure TdxPDFOutline.SetAction(const AValue: TdxPDFCustomAction);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAction));
end;

procedure TdxPDFOutline.SetNext(const AValue: TdxPDFOutline);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FNext));
end;

procedure TdxPDFOutline.ReadColor(ADictionary: TdxPDFDictionary);
var
  AComponents: TDoubleDynArray;
begin
  AComponents := ADictionary.GetDoubleArray(TdxPDFKeywords.OutlineColor);
  if Length(AComponents) = 0 then
    FColor := nil
  else
    FColor := TdxPDFColor.Create(AComponents);
end;

procedure TdxPDFOutline.ReadFlags(ADictionary: TdxPDFDictionary);
const
  Bold = 2;
  Italic = 1;
var
  AFlags: Integer;
begin
  AFlags := ADictionary.GetInteger(TdxPDFKeywords.OutlineFlags, 0);
  FIsItalic := (AFlags and Italic) > 0;
  FIsBold := (AFlags and Bold) > 0;
end;

class function TdxPDFOutline.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Outline;
end;

procedure TdxPDFOutline.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Action := nil;
end;

procedure TdxPDFOutline.DestroySubClasses;
begin
  FDestinationInfo.Finalize;
  FreeAndNil(FColor);
  FreeAndNil(FNext);
  Action := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFOutline.Read(ADictionary: TdxPDFReaderDictionary; AParent: TdxPDFOutlineItem; APrev: TdxPDFOutline);
begin
  Read(ADictionary);
  if ADictionary <> nil then
  begin
    Number := ADictionary.Number;
    FParent := AParent;
    FCatalog := Repository.Catalog;
    FTitle := ADictionary.GetTextString(TdxPDFKeywords.OutlineTitle);
    FAction := ADictionary.GetAction(TdxPDFKeywords.OutlineAction);
    FDestinationInfo := ADictionary.GetDestinationInfo(TdxPDFKeywords.OutlineDestination);
    if (APrev = nil) and (ADictionary.GetDictionary(TdxPDFKeywords.OutlinePrev) <> nil) then
      TdxPDFUtils.RaiseException;
    FPrev := APrev;
    ReadColor(ADictionary);
    ReadFlags(ADictionary);
  end;
end;

{ TdxPDFOutlines }

class function TdxPDFOutlines.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Outlines;
end;

{ TdxPDFOutlineTreeItem }

procedure TdxPDFOutlineTreeItem.Read(AOutline: TdxPDFOutline; AID, AParentID: Integer);
begin
  FOutline := AOutline;
  FID := AID;
  FParentID := AParentID;
  if AOutline <> nil then
  begin
    if AOutline.Color <> nil then
      FColor := dxAlphaColorToColor(TdxPDFUtils.ConvertToAlphaColor(AOutline.Color, 1))
    else
      FColor := dxPDFOutlineTreeItemDefaultColor;
    FTitle := AOutline.Title;
    FIsItalic := AOutline.IsItalic;
    FIsBold := AOutline.IsBold;
    FHasChildren := AOutline.First <> nil;
  end;
end;

function TdxPDFOutlineTreeItem.GetInteractiveOperation: TdxPDFInteractiveOperation;
begin
  if not FInteractiveOperation.IsValid then
    FInteractiveOperation := TdxPDFInteractiveOperation.Create(FOutline.Action, FOutline.Destination);
  Result := FInteractiveOperation;
end;

{ TdxPDFOutlineTree }

function TdxPDFOutlineTree.GetPrintPageNumbers(AItems: TList<TdxPDFOutlineTreeItem>;
  APrintSection: Boolean): TIntegerDynArray;

  procedure Calculate(AOutline: TdxPDFOutline; APageNumbers: TList<Integer>);
  var
    AOutlinePageNumber, ANextOutlinePageNumber, APageNumber: Integer;
  begin
    AOutlinePageNumber := GetPageNumber(AOutline);
    if AOutlinePageNumber = 0 then
      Exit;
    if not APageNumbers.Contains(AOutlinePageNumber) then
      APageNumbers.Add(AOutlinePageNumber);
    if APrintSection then
    begin
      ANextOutlinePageNumber := GetNextPageNumber(AOutline);
      if ANextOutlinePageNumber = 0 then
        Exit;
      if ANextOutlinePageNumber > AOutlinePageNumber then
      begin
        for APageNumber := AOutlinePageNumber + 1 to ANextOutlinePageNumber - 1 do
          if not APageNumbers.Contains(APageNumber) then
            APageNumbers.Add(APageNumber);
      end
      else
        if ANextOutlinePageNumber < AOutlinePageNumber then
          for APageNumber := AOutlinePageNumber - 1 downto ANextOutlinePageNumber + 1 do
            if not APageNumbers.Contains(APageNumber) then
              APageNumbers.Add(APageNumber);
    end;
  end;

  procedure CalculatePageNumbers(AOutline: TdxPDFOutline; APageNumbers: TList<Integer>);
  var
    AChild: TdxPDFOutline;
  begin
    if AOutline <> nil then
    begin
      Calculate(AOutline, APageNumbers);
      AChild := AOutline.First;
      if AChild <> nil then
        CalculatePageNumbers(AChild, APageNumbers);
    end;
  end;

var
  I: Integer;
  APageNumbers: TList<Integer>;
  AItem: TdxPDFOutlineTreeItem;
begin
  SetLength(Result, 0);
  APageNumbers := TList<Integer>.Create;
  try
    for AItem in AItems do
      CalculatePageNumbers(AItem.Outline, APageNumbers);
    APageNumbers.Sort;
    SetLength(Result, APageNumbers.Count);
    for I := 0 to APageNumbers.Count - 1 do
      Result[I] := APageNumbers[I];
  finally
    APageNumbers.Free;
  end;
end;

procedure TdxPDFOutlineTree.Read(ACatalog: TdxPDFCatalog);
var
  AOutlines: TdxPDFOutlines;
begin
  Clear;
  if ACatalog <> nil then
  begin
    FPages := ACatalog.Pages;
    try
      AOutlines := TdxPDFCatalogAccess(ACatalog).Outlines as TdxPDFOutlines;
      if AOutlines <> nil then
        AddOutline(AOutlines.First, 0);
    except
      on EdxPDFException do;
      on EdxPDFAbortException do;
      else
        raise;
    end;
  end;
end;

function TdxPDFOutlineTree.AddOutline(AOutline: TdxPDFOutline; AParentID: Integer): Integer;
var
  AInfo: TdxPDFOutlineTreeItem;
begin
  Result := AParentID;
  while AOutline <> nil do
  begin
    Inc(Result);
    AInfo := TdxPDFOutlineTreeItem.Create;
    AInfo.Read(AOutline, Result, AParentId);
    Add(AInfo);
    if AOutline.First <> nil then
      Result := AddOutline(AOutline.First, Result);
    AOutline := AOutline.Next;
  end;
end;

function TdxPDFOutlineTree.GetNextPageNumber(AOutline: TdxPDFOutline): Integer;
var
  ANextOutline: TdxPDFOutline;
begin
  repeat
    ANextOutline := AOutline.Next;
    if ANextOutline <> nil then
    begin
      Result := GetPageNumber(ANextOutline);
      Exit;
    end;
    AOutline := Safe<TdxPDFOutline>.Cast(AOutline.Parent);
  until AOutline = nil;
  Result := FPages.Count + 1;
end;

function TdxPDFOutlineTree.GetPageNumber(AOutline: TdxPDFOutline): Integer;
var
  ADestination: TdxPDFCustomDestinationAccess;
begin
  ADestination := TdxPDFCustomDestinationAccess(AOutline.ActualDestination);
  if ADestination <> nil then
    Result := ADestination.GetTarget.PageIndex + 1
  else
    Result := 0;
end;

{ TdxPDFJumpAction }

procedure TdxPDFJumpAction.DestroySubClasses;
begin
  FDestinationInfo.Finalize;
  Destination := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFJumpAction.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
    FDestinationInfo := ADictionary.GetDestinationInfo('D');
end;

function TdxPDFJumpAction.GetDestination: TdxPDFCustomDestination;
begin
  if not FDestinationInfo.IsValid then
    Result := nil
  else
  begin
    if FDestination = nil then
      Destination := FDestinationInfo.GetDestination(Catalog, IsInternal);
    Result := FDestination;
  end;
end;

procedure TdxPDFJumpAction.SetDestination(const AValue: TdxPDFCustomDestination);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDestination));
end;

{ TdxPDFGoToAction }

class function TdxPDFGoToAction.GetTypeName: string;
begin
  Result := 'GoTo';
end;

function TdxPDFGoToAction.IsInternal: Boolean;
begin
  Result := True;
end;

procedure TdxPDFGoToAction.Execute(const AController: IdxPDFInteractivityController);
var
  ADestination: TdxPDFCustomDestination;
begin
  ADestination := Destination;
  if ADestination <> nil then
    AController.ShowDocumentPosition(TdxPDFCustomDestinationAccess(ADestination).GetTarget);
end;

{ TdxPDFURIAction }

class function TdxPDFURIAction.GetTypeName: string;
begin
  Result := 'URI';
end;

procedure TdxPDFURIAction.Read(ADictionary: TdxPDFReaderDictionary);
var
  L: Integer;
  AData: TBytes;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    AData := ADictionary.GetBytes(GetTypeName);
    L := Length(AData);
    if L = 0 then
      FURI := ''
    else
      FURI := TdxPDFUtils.ConvertToUTF8String(AData);
  end;
end;

procedure TdxPDFURIAction.Execute(const AController: IdxPDFInteractivityController);
begin
  AController.OpenURI(URI);
end;

{ TdxPDFNamedAction }

class function TdxPDFNamedAction.GetTypeName: string;
begin
  Result := TdxPDFKeywords.ActionNamed;
end;

procedure TdxPDFNamedAction.Execute(const AController: IdxPDFInteractivityController);
begin
  if FActionName = 'NextPage' then
    AController.GoToNextPage
  else
    if FActionName = 'PrevPage' then
      AController.GoToPrevPage
    else
      if FActionName = 'FirstPage' then
        AController.GoToFirstPage
      else
        if FActionName = 'LastPage' then
          AController.GoToLastPage;
end;

procedure TdxPDFNamedAction.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
    FActionName := ADictionary.GetString(TdxPDFKeywords.ActionName);
end;

{ TdxPDFEmbeddedGoToAction }

class function TdxPDFEmbeddedGoToAction.GetTypeName: string;
begin
  Result := 'GoToE';
end;

{ TdxPDFGoTo3dViewAction }

class function TdxPDFGoTo3dViewAction.GetTypeName: string;
begin
  Result := 'GoTo3DView';
end;

{ TdxPDFHideAction }

class function TdxPDFHideAction.GetTypeName: string;
begin
  Result := 'Hide';
end;

{ TdxPDFImportDataAction }

class function TdxPDFImportDataAction.GetTypeName: string;
begin
  Result := 'ImportData';
end;

{ TdxPDFJavaScriptAction }

class function TdxPDFJavaScriptAction.GetTypeName: string;
begin
  Result := 'JavaScript';
end;

{ TdxPDFLaunchAction }

class function TdxPDFLaunchAction.GetTypeName: string;
begin
  Result := 'Launch';
end;

{ TdxPDFMovieAction }

class function TdxPDFMovieAction.GetTypeName: string;
begin
  Result := 'Movie';
end;

{ TdxPDFRemoteGoToAction }

class function TdxPDFRemoteGoToAction.GetTypeName: string;
begin
  Result := 'GoToR';
end;

{ TdxPDFRenditionAction }

class function TdxPDFRenditionAction.GetTypeName: string;
begin
  Result := 'Rendition';
end;

{ TdxPDFResetFormAction }

class function TdxPDFResetFormAction.GetTypeName: string;
begin
  Result := 'ResetForm';
end;

{ TdxPDFSetOCGStateAction }

class function TdxPDFSetOCGStateAction.GetTypeName: string;
begin
  Result := 'SetOCGState';
end;

{ TdxPDFSoundAction }

class function TdxPDFSoundAction.GetTypeName: string;
begin
  Result := 'Sound';
end;

{ TdxPDFSubmitFormAction }

class function TdxPDFSubmitFormAction.GetTypeName: string;
begin
  Result := 'SubmitForm';
end;

{ TdxPDFThreadAction }

class function TdxPDFThreadAction.GetTypeName: string;
begin
  Result := 'Thread';
end;

{ TdxPDFTransitionAction }

class function TdxPDFTransitionAction.GetTypeName: string;
begin
  Result := 'Trans';
end;

{ TdxPDFActionAnnotation }

procedure TdxPDFActionAnnotation.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Action := nil;
end;

procedure TdxPDFActionAnnotation.DestroySubClasses;
begin
  FDestinationInfo.Finalize;
  Action := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFActionAnnotation.Resolve(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Resolve(ADictionary);
  if ADictionary <> nil then
  begin
    Action := ADictionary.GetAction('A');
    FDestinationInfo := ADictionary.GetDestinationInfo('Dest');
    if (FAction <> nil) and FDestinationInfo.IsValid then
      TdxPDFUtils.Abort;
  end;
end;

function TdxPDFActionAnnotation.GetDestination: TdxPDFCustomDestination;
begin
  if FDestinationInfo.IsValid then
    Result := FDestinationInfo.GetDestination(Repository.Catalog, True)
  else
    Result := nil;
end;

function TdxPDFActionAnnotation.GetInteractiveOperation: TdxPDFInteractiveOperation;
begin
  if not FInteractiveOperation.IsValid then
    FInteractiveOperation := TdxPDFInteractiveOperation.Create(FAction, GetDestination);
  Result := FInteractiveOperation;
end;

procedure TdxPDFActionAnnotation.SetAction(const AValue: TdxPDFCustomAction);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAction));
end;

{ TdxPDFWidgetAnnotation }

class function TdxPDFWidgetAnnotation.GetTypeName: string;
begin
  Result := 'Widget';
end;

function TdxPDFWidgetAnnotation.CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField;
begin
  if InteractiveFormField <> nil then
    Result := TdxPDFInteractiveFormCustomFieldAccess(InteractiveFormField).CreateAcroField(AState)
  else
    Result := nil;
end;

function TdxPDFWidgetAnnotation.CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject;
begin
  Result := nil;
  if InteractiveFormField <> nil then
    Result := TdxPDFInteractiveFormCustomFieldAccess(InteractiveFormField).CreateAppearanceBuilder(AState);
end;

function TdxPDFWidgetAnnotation.CreateAppearanceForm(const AName: string): TdxPDFForm;
begin
  Result := inherited CreateAppearanceForm(AName);
  Result.Matrix := CreateFormTransformationMatrix;
end;

function TdxPDFWidgetAnnotation.GetUseDefaultForm: Boolean;
begin
  Result := (FInteractiveFormField <> nil) and FInteractiveFormField.UseDefaultAppearanceForm;
end;

function TdxPDFWidgetAnnotation.NeedCreateAppearance(AForm: TdxPDFForm): Boolean;
begin
  Result := inherited NeedCreateAppearance(AForm) or (FInteractiveFormField <> nil) and
    (TdxPDFInteractiveFormCustomFieldAccess(FInteractiveFormField).Form <> nil) and
    TdxPDFInteractiveFormCustomFieldAccess(FInteractiveFormField).Form.NeedAppearances;
end;

procedure TdxPDFWidgetAnnotation.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Action := nil;
  BorderStyle := nil;
  InteractiveFormField := nil;
end;

procedure TdxPDFWidgetAnnotation.DestroySubClasses;
begin
  FreeAndNil(FAppearanceCharacteristics);
  InteractiveFormField := nil;
  BorderStyle := nil;
  Action := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFWidgetAnnotation.Read(ADictionary: TdxPDFReaderDictionary; APage: TdxPDFPage);
begin
  inherited Read(ADictionary, APage);
  if Dictionary <> nil then
  begin
    FHighlightingMode := Dictionary.GetAnnotationHighlightingMode;
    ReadAppearanceCharacteristics;
    BorderStyle := TdxPDFAnnotationBorderStyle.Parse(Dictionary);
  end;
end;

function TdxPDFWidgetAnnotation.GetAppearanceContentRectangle: TdxRectF;
var
  AFormBBox: TdxRectF;
  ALayoutBorderWidth, ARight, ATop: Double;
begin
  AFormBBox := GetAppearanceFormBoundingBox;
  ALayoutBorderWidth := BorderWidth;
  ARight := AFormBBox.Width - ALayoutBorderWidth;
  ATop := Abs(AFormBBox.Height) - ALayoutBorderWidth;
  Result := TdxRectF.CreateSize(ALayoutBorderWidth, ALayoutBorderWidth,
    IfThen(ARight > ALayoutBorderWidth, ARight, ALayoutBorderWidth + 1),
    IfThen(ATop > ALayoutBorderWidth, ATop, ALayoutBorderWidth + 1));
end;

function TdxPDFWidgetAnnotation.GetBackgroundColor: TdxPDFColor;
begin
  if AppearanceCharacteristics = nil then
    Result := nil
  else
    Result := AppearanceCharacteristics.BackgroundColor;
end;

function TdxPDFWidgetAnnotation.GetBorderWidth: Single;
begin
  if BorderStyle <> nil then
    Result := BorderStyle.Width
  else
    if Border <> nil then
      Result := Border.LineWidth
    else
      Result := 0;
end;

procedure TdxPDFWidgetAnnotation.SetAppearanceCharacteristics(const AValue: TdxPDFWidgetAppearanceCharacteristics);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAppearanceCharacteristics));
end;

procedure TdxPDFWidgetAnnotation.SetBorderStyle(const AValue: TdxPDFAnnotationBorderStyle);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FBorderStyle));
end;

procedure TdxPDFWidgetAnnotation.SetInteractiveFormField(const AValue: TdxPDFInteractiveFormField);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FInteractiveFormField));
end;

function TdxPDFWidgetAnnotation.CreateFormTransformationMatrix: TdxPdfTransformationMatrix;
var
  ARotationAngle: Integer;
begin
  ARotationAngle := 0;
  if FAppearanceCharacteristics <> nil then
    ARotationAngle := FAppearanceCharacteristics.RotationAngle;
  case TdxPDFUtils.NormalizeRotate(ARotationAngle) of
    90:
      Result := TdxPdfTransformationMatrix.CreateEx(0, 1, -1, 0, Rect.Width, 0);
    180:
      Result := TdxPdfTransformationMatrix.CreateEx(-1, 0, 0, -1, Rect.Width, Abs(Rect.Height));
    270:
      Result := TdxPdfTransformationMatrix.CreateEx(0, -1, 1, 0, 0, Abs(Rect.Height));
  else
    Result := TdxPdfTransformationMatrix.Create;
  end;
end;

procedure TdxPDFWidgetAnnotation.ReadAppearanceCharacteristics;
var
  AAppearanceCharacteristicsDictionary: TdxPDFReaderDictionary;
begin
  if Dictionary.TryGetDictionary('MK', AAppearanceCharacteristicsDictionary) then
  begin
    AppearanceCharacteristics := TdxPDFWidgetAppearanceCharacteristics.Create(nil);
    AppearanceCharacteristics.Read(AAppearanceCharacteristicsDictionary);
  end;
end;

{ TdxPDFLinkAnnotation }

class function TdxPDFLinkAnnotation.GetTypeName: string;
begin
  Result := 'Link';
end;

function TdxPDFLinkAnnotation.CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField;
begin
  Result := TdxPDFHyperlink.Create;
  TdxPDFAnnotationFieldAccess(Result).DocumentState := AState;
  TdxPDFAnnotationFieldAccess(Result).SetAnnotation(Self);
end;

procedure TdxPDFLinkAnnotation.Resolve(ADictionary: TdxPDFReaderDictionary);
begin
  FCatalog := ADictionary.Repository.Catalog;
  inherited Resolve(ADictionary);
end;

function TdxPDFLinkAnnotation.GetBounds: TdxRectF;
begin
  Result := Rect;
end;

function TdxPDFLinkAnnotation.GetHint: string;
begin
  if (Action <> nil) and (Action is TdxPDFURIAction) then
    Result := TdxPDFURIAction(Action).URI
  else
    Result := '';
end;

{ TdxPDFMarkupAnnotation }

function TdxPDFMarkupAnnotation.CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField;
begin
  Result := GetAnnotationFieldClass.Create;
  TdxPDFAnnotationFieldAccess(Result).DocumentState := AState;
  TdxPDFAnnotationFieldAccess(Result).SetAnnotation(Self);
end;

procedure TdxPDFMarkupAnnotation.Resolve(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Resolve(ADictionary);
  if ADictionary <> nil then
  begin
    FSubject := ADictionary.GetTextString('Subj');
    FTitle := ADictionary.GetTextString('T');
    FOpacity := ADictionary.GetDouble('CA', 1.0);
  end;
end;

function TdxPDFMarkupAnnotation.GetAnnotationFieldClass: TdxPDFAnnotationFieldClass;
begin
  Result := TdxPDFAnnotationField;
end;

function TdxPDFMarkupAnnotation.GetBounds: TdxRectF;
begin
  Result := Rect;
end;

function TdxPDFMarkupAnnotation.GetHint: string;
begin
  if Length(Contents) > 0 then
    Result := Title + dxCRLF + Contents
  else
    Result := '';
end;

function TdxPDFMarkupAnnotation.GetTitle: string;
begin
  Ensure;
  Result := FTitle;
end;

{ TdxPDFFileAttachmentAnnotation }

class function TdxPDFFileAttachmentAnnotation.GetTypeName: string;
begin
  Result := 'FileAttachment';
end;

function TdxPDFFileAttachmentAnnotation.GetAnnotationFieldClass: TdxPDFAnnotationFieldClass;
begin
  Result := TdxPDFFileAttachmentAnnotationField;
end;

procedure TdxPDFFileAttachmentAnnotation.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FileSpecification := nil;
end;

procedure TdxPDFFileAttachmentAnnotation.DestroySubClasses;
begin
  InternalSetFileSpecification(nil);
  inherited DestroySubClasses;
end;

procedure TdxPDFFileAttachmentAnnotation.Resolve(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Resolve(ADictionary);
  if ADictionary <> nil then
  begin
    FileSpecification := TdxPDFFileSpecification.Parse(ADictionary.GetDictionary('FS'));
    if not ADictionary.TryGetString('Name', FIconName) then
      FIconName := 'PushPin';
  end;
end;

function TdxPDFFileAttachmentAnnotation.GetFileSpecification: TdxPDFFileSpecification;
begin
  Ensure;
  Result := FFileSpecification;
end;

procedure TdxPDFFileAttachmentAnnotation.SetFileSpecification(const AValue: TdxPDFFileSpecification);
begin
  InternalSetFileSpecification(AValue);
end;

procedure TdxPDFFileAttachmentAnnotation.InternalSetFileSpecification(const AValue: TdxPDFFileSpecification);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFileSpecification));
end;

{ TdxPDFWidgetAppearanceCharacteristics }

procedure TdxPDFWidgetAppearanceCharacteristics.DestroySubClasses;
begin
  dxPDFFreeObject(FBackgroundColor);
  dxPDFFreeObject(FBorderColor);
  inherited DestroySubClasses;
end;

procedure TdxPDFWidgetAppearanceCharacteristics.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FCaption := ADictionary.GetString('CA');
  FBorderColor := ADictionary.GetColor('BC');
  FBackgroundColor := ADictionary.GetColor('BG');
  FRotationAngle := TdxPDFUtils.NormalizeRotate(ADictionary.GetInteger('R', 0));
end;

{ TdxPDFTextFormField }

function TdxPDFTextFormField.GetValue: Variant;
begin
  Result := FText;
end;

class function TdxPDFTextFormField.GetTypeName: string;
begin
  Result := 'Tx';
end;

function TdxPDFTextFormField.CreateAppearanceBuilder(AState: TdxPDFDocumentState): TObject;
begin
  Result := TdxPDFTextFormFieldAppearanceBuilder.Create(Widget as TdxPDFWidgetAnnotation, Self, AState);
end;

function TdxPDFTextFormField.GetAcroFieldClass: TdxPDFAcroFormFieldClass;
begin
  Result := TdxPDFAcroFormTextBoxField;
end;

procedure TdxPDFTextFormField.Read(AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField;
  ADictionary: TdxPDFReaderDictionary; ANumber: Integer);
begin
  inherited Read(AForm, AParent, ADictionary, ANumber);
  if ADictionary <> nil then
  begin
    FText := GetText(ADictionary, 'V');
    FDefaultText := GetText(ADictionary, 'DV');
    FMaxLen := ADictionary.GetInteger('MaxLen');
    if ValuesProvider = nil then
      FValuesProvider := Self
    else
      FValuesProvider := ValuesProvider as TdxPDFTextFormField;
  end;
end;

function TdxPDFTextFormField.GetDefaultText: string;
begin
  Result := FValuesProvider.FDefaultText;
end;

function TdxPDFTextFormField.GetMaxLen: Integer;
begin
  Result := FValuesProvider.FMaxLen;
end;

function TdxPDFTextFormField.GetText: string;
begin
  Result := FValuesProvider.FText;
end;

function TdxPDFTextFormField.GetText(ADictionary: TdxPDFReaderDictionary; const AKey: string): string;
begin
  Result := ADictionary.GetTextString(AKey);
end;

procedure TdxPDFTextFormField.InternalSetValue(const AValue: Variant; AState: TdxPDFDocumentState);
begin
  if not SameText(FText, AValue) then
  begin
    FText := AValue;
    Changed(AState);
  end;
end;

{ TdxPDFSignatureFormField }

class function TdxPDFSignatureFormField.GetTypeName: string;
begin
  Result := 'Sig';
end;

{ TdxPDFChoiceField }

class function TdxPDFChoiceField.GetTypeName: string;
begin
  Result := 'Ch';
end;

{ TdxPDFButtonField }

class function TdxPDFButtonField.GetTypeName: string;
begin
  Result := 'Btn';
end;

function TdxPDFButtonField.GetAcroFieldClass: TdxPDFAcroFormFieldClass;
begin
  Result := TdxPDFAcroFormButton;
end;

function TdxPDFButtonField.UseDefaultAppearanceForm: Boolean;
begin
  Result := (Integer(Flags) and Integer(ffPushButton)) <> 0;
end;

{ TdxPDFAcroFormVisualField }

constructor TdxPDFAcroFormVisualField.Create;
begin
  inherited Create;
  FVisible := True;
  FPrint := True;
end;

function TdxPDFAcroFormVisualField.GetFlags: TdxPDFInteractiveFormFieldFlags;
begin
  Result := inherited Flags;
  if FReadOnly then
    Result := TdxPDFInteractiveFormFieldFlags(Integer(Result) or Integer(ffReadOnly));
  if FRequired then
    Result := TdxPDFInteractiveFormFieldFlags(Integer(Result) or Integer(ffRequired));
end;

function TdxPDFAcroFormVisualField.GetAnnotationFlags: TdxPDFAnnotationFlags;
begin
  Result := afNone;
  if not FVisible then
    Result := TdxPDFAnnotationFlags(Integer(Result) or Integer(afNoView));
  if FPrint then
    Result := TdxPDFAnnotationFlags(Integer(Result) or Integer(afPrint));
end;

{ TdxPDFAcroFormTextBoxField }

function TdxPDFAcroFormTextBoxField.GetText: string;
begin
  Result := Field.GetValue;
end;

procedure TdxPDFAcroFormTextBoxField.SetMaxLength(const AValue: Integer);
begin
  FMaxLength := Max(0, AValue);
end;

procedure TdxPDFAcroFormTextBoxField.SetText(const AValue: string);
begin
  Field.SetValue(AValue, DocumentState);
end;

function TdxPDFAcroFormTextBoxField.GetHint: string;
begin
  Result := TdxPDFInteractiveFormCustomFieldAccess(Field).AlternateName;
end;

function TdxPDFAcroFormTextBoxField.GetHitCode: Integer;
begin
  Result := hcTextBox;
end;

{ TdxPDFAcroFormButton }

function TdxPDFAcroFormButton.GetHint: string;
var
  AWidget: TdxPDFWidgetAnnotation;
begin
  AWidget := TdxPDFInteractiveFormCustomFieldAccess(Field).Widget as TdxPDFWidgetAnnotation;
  if AWidget <> nil then
    Result := AWidget.AppearanceCharacteristics.Caption
  else
    Result := inherited GetHint;
  if (Result <> '') and not GetInteractiveOperation.IsValid then
    Result := '';
end;

function TdxPDFAcroFormButton.GetHitCode: Integer;
begin
  Result := hcButton;
end;

initialization
  dxPDFRegisterDocumentObjectClass(TdxPDFFitDestination);
  dxPDFRegisterDocumentObjectClass(TdxPDFFitRectangleDestination);
  dxPDFRegisterDocumentObjectClass(TdxPDFFitHorizontallyDestination);
  dxPDFRegisterDocumentObjectClass(TdxPDFFitVerticallyDestination);
  dxPDFRegisterDocumentObjectClass(TdxPDFXYZDestination);
  dxPDFRegisterDocumentObjectClass(TdxPDFFitBBoxDestination);
  dxPDFRegisterDocumentObjectClass(TdxPDFFitBBoxHorizontallyDestination);
  dxPDFRegisterDocumentObjectClass(TdxPDFFitBBoxVerticallyDestination);

  dxPDFRegisterDocumentObjectClass(TdxPDFGoToAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFURIAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFEmbeddedGoToAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFGoTo3dViewAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFHideAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFImportDataAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFJavaScriptAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFLaunchAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFNamedAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFRemoteGoToAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFRenditionAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFResetFormAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFSetOCGStateAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFSoundAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFSubmitFormAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFThreadAction);
  dxPDFRegisterDocumentObjectClass(TdxPDFTransitionAction);

  dxPDFRegisterDocumentObjectClass(TdxPDFSignatureFormField);
  dxPDFRegisterDocumentObjectClass(TdxPDFTextFormField);
  dxPDFRegisterDocumentObjectClass(TdxPDFChoiceField);
  dxPDFRegisterDocumentObjectClass(TdxPDFButtonField);

  dxPDFRegisterDocumentObjectClass(TdxPDFLinkAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFWidgetAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFFileAttachmentAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.CircleAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.SquareAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.FreeTextAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.RedactAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.TextAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.StampAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.InkAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.UnderlineAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.SquigglyAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.StrikeOutAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.HighlightAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.CaretAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.PolygonAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.PolygonAnnotation, TdxPDFMarkupAnnotation);
  dxPDFRegisterDocumentObjectClass(TdxPDFKeywords.LineAnnotation, TdxPDFMarkupAnnotation);

finalization
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.LineAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.PolygonAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.PolyLineAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.CaretAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.HighlightAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.StrikeOutAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.SquigglyAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.UnderlineAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.InkAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.StampAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.TextAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.RedactAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.FreeTextAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.SquareAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFKeywords.CircleAnnotation, TdxPDFMarkupAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFFileAttachmentAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFWidgetAnnotation);
  dxPDFUnregisterDocumentObjectClass(TdxPDFLinkAnnotation);

  dxPDFUnregisterDocumentObjectClass(TdxPDFButtonField);
  dxPDFUnregisterDocumentObjectClass(TdxPDFChoiceField);
  dxPDFUnregisterDocumentObjectClass(TdxPDFTextFormField);
  dxPDFUnregisterDocumentObjectClass(TdxPDFSignatureFormField);

  dxPDFUnregisterDocumentObjectClass(TdxPDFTransitionAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFThreadAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFSubmitFormAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFSoundAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFSetOCGStateAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFResetFormAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFRenditionAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFRemoteGoToAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFNamedAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFLaunchAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFJavaScriptAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFImportDataAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFHideAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFGoTo3dViewAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFEmbeddedGoToAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFURIAction);
  dxPDFUnregisterDocumentObjectClass(TdxPDFGoToAction);

  dxPDFUnregisterDocumentObjectClass(TdxPDFFitBBoxVerticallyDestination);
  dxPDFUnregisterDocumentObjectClass(TdxPDFFitBBoxHorizontallyDestination);
  dxPDFUnregisterDocumentObjectClass(TdxPDFFitBBoxDestination);
  dxPDFUnregisterDocumentObjectClass(TdxPDFXYZDestination);
  dxPDFUnregisterDocumentObjectClass(TdxPDFFitVerticallyDestination);
  dxPDFUnregisterDocumentObjectClass(TdxPDFFitHorizontallyDestination);
  dxPDFUnregisterDocumentObjectClass(TdxPDFFitRectangleDestination);
  dxPDFUnregisterDocumentObjectClass(TdxPDFFitDestination);

end.

