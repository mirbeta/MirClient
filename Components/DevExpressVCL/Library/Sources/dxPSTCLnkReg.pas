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

unit dxPSTCLnkReg;

interface

{$I cxVer.inc}

uses
  DesignIntf, DesignEditors, Classes, dxPSReg;

type
  { TdxTeeChartReportLinkGraphicClassNamePropertyEditor }

  TdxTeeChartReportLinkGraphicClassNamePropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;
implementation

uses
 {$IFDEF USEJPEGIMAGE}
  Jpeg,
 {$ENDIF}
  Graphics, dxPSTCLnk, Chart;

{ TdxTeeChartReportLinkGraphicClassNamePropertyEditor }

function TdxTeeChartReportLinkGraphicClassNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList]
end;

procedure TdxTeeChartReportLinkGraphicClassNamePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc(TBitmap.ClassName);
  Proc(TMetafile.ClassName);
 {$IFDEF USEJPEGIMAGE}
  Proc(TJPEGImage.ClassName);
 {$ENDIF}
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterNoIcon([TdxTeeChartReportLink]);
  dxPSRegisterReportLinkUnit('dxPSTCLnk', TCustomdxTeeChartReportLink);
  dxPSRegisterReportLinkUnit('dxPSTCLnk', TdxTeeChartReportLink);
  RegisterPropertyEditor(TypeInfo(string), TCustomdxTeeChartReportLink,
    'GraphicClassName', TdxTeeChartReportLinkGraphicClassNamePropertyEditor);
end;

end.
