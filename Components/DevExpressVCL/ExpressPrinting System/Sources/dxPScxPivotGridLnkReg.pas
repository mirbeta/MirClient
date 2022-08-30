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

unit dxPScxPivotGridLnkReg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignIntf, DesignEditors, Windows, Classes, Controls, dxPSRes, dxPSUtl,
  cxStyles, cxCustomPivotGrid, cxPivotGrid, dxPScxPivotGridLnk,
  cxStyleSheetEditor, SysUtils, cxPivotGridStyleSheetsPreview, dxPSReg;

type
  { TcxPivotGridReportLinkStyleSheetEditorPreview }

  TcxPivotGridReportLinkStyleSheetEditorPreview = class(TcxPivotGridStyleSheetEditorPreview)
  public
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;

    class procedure Register;
    class procedure Unregister;
  end;

{ TcxPivotGridReportLinkStyleSheetEditorPreview }

class function TcxPivotGridReportLinkStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxPivotGridReportLinkStyleSheet;
end;

procedure TcxPivotGridReportLinkStyleSheetEditorPreview.SetStyleSheet(
  AStyleSheet: TcxCustomStyleSheet);
var
  AStyles: TcxPivotGridReportLinkStyles;
begin
  if AStyleSheet is TcxPivotGridReportLinkStyleSheet then
  begin
    AStyles := TcxPivotGridReportLinkStyleSheet(AStyleSheet).Styles;
    with PivotGrid.Styles do
    begin
      ColumnHeader := AStyles.ColumnHeader;
      Content := AStyles.Content;
      FieldHeader := AStyles.FieldHeader;
      FilterHeaderArea := AStyles.HeaderBackground;
      RowHeader := AStyles.RowHeader;
    end;
  end
  else
    PivotGrid.Styles.ResetStyles;
end;

class procedure TcxPivotGridReportLinkStyleSheetEditorPreview.Register;
begin
  RegisterStyleSheetEditorPreview(Self);
end;

class procedure TcxPivotGridReportLinkStyleSheetEditorPreview.Unregister;
begin
  UnregisterStyleSheetEditorPreview(Self);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterNoIcon([TcxPivotGridReportLink, TcxPivotGridReportLinkStyleSheet]);
  dxPSRegisterReportLinkUnit('dxPScxPivotGridLnk', TcxPivotGridReportLink);
  RegisterClasses([TcxPivotGridReportLink,
    TcxPivotGridReportLinkStyles, TcxPivotGridReportLinkStyleSheet]);
  RegisterStyleSheetClass(TcxPivotGridReportLinkStyleSheet);
end;

initialization
  TcxPivotGridReportLinkStyleSheetEditorPreview.Register;

finalization
  TcxPivotGridReportLinkStyleSheetEditorPreview.Unregister;

end.
