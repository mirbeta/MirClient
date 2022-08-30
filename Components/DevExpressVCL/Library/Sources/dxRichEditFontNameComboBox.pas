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

unit  dxRichEditFontNameComboBox;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Variants, Windows, Classes, Controls, Dialogs, Forms, Graphics, Messages,
  Printers, SysUtils,
  dxCore, dxCoreClasses, cxClasses, cxContainer, cxControls, cxGraphics, cxButtons,
  cxDataStorage, cxVariants, cxEdit, cxTextEdit, cxDropDownEdit, cxEditUtils,
  cxExtEditConsts, cxExtEditUtils, cxImageComboBox, cxLookAndFeels, cxMaskEdit,
  cxFilterControlUtils, cxFontNameComboBox;

type
  TdxCustomRichEditFontNameComboBoxProperties = class(TcxCustomFontNameComboBoxProperties)
  public
    constructor Create(AOwner: TPersistent); override;
    procedure LoadFontNames; override;
  end;

  TdxRichEditFontNameComboBoxProperties = class(TdxCustomRichEditFontNameComboBoxProperties)
  published
    property Alignment;
    property AssignedValues;
    property BeepOnError;
    property ButtonGlyph;
    property CharCase;
    property ClearKey;
    property DropDownAutoWidth;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
    property FontPreview;
    property FontTypes;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property ImmediateUpdateText;
    property ItemHeight;
    property MaxMRUFonts default 6;
    property OEMConvert;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property UseOwnFont default True;
    property ValidateOnEnter;
    property ValidationOptions;
    property OnAddedMRUFont;
    property OnChange;
    property OnCloseUp;
    property OnDeletedMRUFont;
    property OnDrawItem;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnLoadFontComplete;
    property OnMeasureItem;
    property OnMovedMRUFont;
    property OnNewLookupDisplayText;
    property OnPopup;
    property OnValidate;
  end;

  TdxRichEditFontNameComboBox = class(TcxCustomFontNameComboBox)
  private
    function GetActiveProperties: TdxRichEditFontNameComboBoxProperties;
    function GetProperties: TdxRichEditFontNameComboBoxProperties;
    procedure SetProperties(Value: TdxRichEditFontNameComboBoxProperties);
  protected
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxRichEditFontNameComboBoxProperties
      read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TdxRichEditFontNameComboBoxProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  dxRichEdit.Platform.Win.FontCache;

{ TcxCustomRichEditFontNameComboBoxProperties }

constructor TdxCustomRichEditFontNameComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  DropDownListStyle := lsEditList;
  UseOwnFont := True;
  MaxMRUFonts := 6;
  ShowFontTypeIcon := [];
  FontPreview.Visible := False;
end;

procedure TdxCustomRichEditFontNameComboBoxProperties.LoadFontNames;
var
  AStrings: TStrings;
  AValue: TdxTrueTypeFontInfo;
  I: Integer;
begin
  AStrings := TdxGdiFontCache.CreateSystemTrueTypeFonts;
  try
    FontItems.BeginUpdate;
    FontItems.Clear;
    for I := 0 to AStrings.Count - 1 do
    begin
      AValue := TdxTrueTypeFontInfo(AStrings.Objects[I]);
      FontItems.AddObject(AValue.FontName, TObject(AValue.FontType));
    end;
    FontItems.EndUpdate;
  finally
    AStrings.Free;
  end;
end;

{ TdxRichEditFontNameComboBox }

function TdxRichEditFontNameComboBox.GetActiveProperties: TdxRichEditFontNameComboBoxProperties;
begin
  Result := TdxRichEditFontNameComboBoxProperties(InternalGetActiveProperties);
end;

function TdxRichEditFontNameComboBox.GetProperties: TdxRichEditFontNameComboBoxProperties;
begin
  Result := TdxRichEditFontNameComboBoxProperties(inherited Properties);
end;

class function TdxRichEditFontNameComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxRichEditFontNameComboBoxProperties;
end;

procedure TdxRichEditFontNameComboBox.SetProperties(Value: TdxRichEditFontNameComboBoxProperties);
begin
  Properties.Assign(Value);
end;

end.
