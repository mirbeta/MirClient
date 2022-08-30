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

unit dxPSDBTCLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  VCLTee.DBChart,
{$ELSE}
  DBChart,
{$ENDIF}
  Classes, dxPSTCLnk;

type
  TdxDBTeeChartReportLink = class(TCustomdxTeeChartReportLink)
  private
    function GetDBChart: TDBChart;
  public
    property DBChart: TDBChart read GetDBChart;
    property GraphicClass;
  published
    property GraphicClassName;
    property OnCreateGraphic;
    property OnGetGraphicClass;
  end;

implementation

uses
  dxPSCore;

function TdxDBTeeChartReportLink.GetDBChart: TDBChart;
begin
  Result := inherited Chart as TDBChart;
end;

initialization
  dxPSRegisterReportLink(TdxDBTeeChartReportLink, TDBChart, nil);

finalization
  dxPSUnregisterReportLink(TdxDBTeeChartReportLink, TDBChart, nil);

end.
