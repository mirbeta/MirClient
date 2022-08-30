{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPScxCommonReg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignIntf, DesignEditors, SysUtils, TypInfo, Classes, cxClasses, dxPSReg,
  cxLookAndFeels, dxPScxCommon;

type
  { TdxReportLinkLookAndFeelKindPropertyEditor }

  TdxReportLinkLookAndFeelKindPropertyEditor = class(TEnumProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxReportLinkOptionsPropertyEditor }

  TdxReportLinkOptionsPropertyEditor = class(TClassProperty)
  private
    function GetOptions: TdxCustomReportLinkOptions;
    function GetReportLink: TdxCustomcxControlReportLink;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    property Options: TdxCustomReportLinkOptions read GetOptions;
    property ReportLink: TdxCustomcxControlReportLink read GetReportLink;
  end;

  { TdxReportLinkStylesPropertyEditor }

  TdxReportLinkStylesPropertyEditor = class(TClassProperty)
  private
    function GetReportLink: TdxCustomcxControlReportLink;
    function GetStyles: TdxCustomReportLinkStyles;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    property Styles: TdxCustomReportLinkStyles read GetStyles;
    property ReportLink: TdxCustomcxControlReportLink read GetReportLink;
  end;

{ TdxReportLinkLookAndFeelKindPropertyEditor }

procedure TdxReportLinkLookAndFeelKindPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Integer(lfFlat) to Integer(lfUltraFlat) do
    Proc(GetEnumName(GetPropType, I));
end;

{ TdxReportLinkOptionsPropertyEditor }

procedure TdxReportLinkOptionsPropertyEditor.Edit;
var
  DesignerSupport: IdxReportLinkOptionsDesignerSupport;
begin
  if Supports(TObject(Options), IdxReportLinkOptionsDesignerSupport, DesignerSupport) then
  begin
    DesignerSupport.DesignerInitialize;
    try
      ReportLink.DesignReport;
    finally
      DesignerSupport.DesignerFinalize;
    end;
  end;
end;

function TdxReportLinkOptionsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog]; // TODO: ReportLink.CanDesign
end;

function TdxReportLinkOptionsPropertyEditor.GetOptions: TdxCustomReportLinkOptions;
begin
  Result := TdxCustomReportLinkOptions(GetOrdValue);
end;

function TdxReportLinkOptionsPropertyEditor.GetReportLink: TdxCustomcxControlReportLink;
begin
  Result := TdxCustomcxControlReportLink(GetComponent(0));
end;

{ TdxReportLinkStylesPropertyEditor }

procedure TdxReportLinkStylesPropertyEditor.Edit;
var
  DesignerSupport: IdxReportLinkOptionsDesignerSupport;
begin
  if Supports(TObject(Styles), IdxReportLinkOptionsDesignerSupport, DesignerSupport) then
  begin
    DesignerSupport.DesignerInitialize;
    try
      ReportLink.DesignReport;
    finally
      DesignerSupport.DesignerFinalize;
    end;
  end;
end;

function TdxReportLinkStylesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog]; // TODO: ReportLink.CanDesign
end;

function TdxReportLinkStylesPropertyEditor.GetReportLink: TdxCustomcxControlReportLink;
begin
  Result := TdxCustomcxControlReportLink(GetComponent(0));
end;

function TdxReportLinkStylesPropertyEditor.GetStyles: TdxCustomReportLinkStyles;
begin
  Result := TdxCustomReportLinkStyles(GetOrdValue);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterPropertyEditor(TypeInfo(TcxLookAndFeelKind), TdxCustomReportLinkOptionsFormatting,
    'LookAndFeelKind', TdxReportLinkLookAndFeelKindPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxCustomReportLinkOptions),
    TdxCustomcxControlReportLink, '', TdxReportLinkOptionsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxCustomReportLinkStyles),
    TdxCustomcxControlReportLink, '', TdxReportLinkStylesPropertyEditor);
end;

end.
