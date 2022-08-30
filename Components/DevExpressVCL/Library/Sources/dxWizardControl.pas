{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxWizardControl;

{$I cxVer.inc}

interface

uses
  Classes, dxCustomWizardControl;

type

  { TdxWizardControlPage }

  TdxWizardControlPage = class(TdxWizardControlCustomPage)
  published
    property BiDiMode;
    property DoubleBuffered;
    property Font;
    property Header;
    property OptionsSize;
    property PageIndex;
    property PageVisible;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property Watermark;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;

  { TdxWizardControl }

  TdxWizardControl = class(TdxCustomWizardControl)
  public
    function AddPage: TdxWizardControlPage; overload;
  published
    property ActivePage stored False;
    property Align;
    property AutoSize;
    property BiDiMode;
    property Buttons;
    property Font;
    property Header;
    property InfoPanel;
    property LookAndFeel;
    property OptionsAnimate;
    property OptionsViewStyleAero;
    property ParentBiDiMode;
    property ParentFont;
    property ViewStyle;
    property Watermark;

    property OnButtonClick;
    property OnClick;
    property OnDblClick;
    property OnHandleChildControlKey;
    property OnInfoPanelClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPageChanged;
    property OnPageChanging;
    property OnResize;
  end;

implementation

{ TdxWizardControl }

function TdxWizardControl.AddPage: TdxWizardControlPage;
begin
  Result := TdxWizardControlPage(AddPage(TdxWizardControlPage));
end;

initialization
  RegisterClasses([TdxWizardControlPage]);
end.
