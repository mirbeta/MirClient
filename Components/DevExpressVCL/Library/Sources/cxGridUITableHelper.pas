{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridUITableHelper;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, cxGrid, cxGridCustomView, cxGridUIHelper,
  cxGridCustomTableView, cxGridUICustomTableHelper;

type
  TcxGridTableViewOperationHelper = class(TcxGridCustomTableViewOperationHelper)
  protected
    procedure RegisterOperations; override;
    procedure DoShowColumnsCustomizing(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowGroupingPanel(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowSummaryFooter(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowGrid(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoColumnAutoWidth(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowPreview(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowHeaders(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowColumnCustomize(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoInvertSelect(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowIndicator(const AParameter: TcxCustomGridOperationHelperParameters);
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
    function IsOperationEnabled(AView: TcxCustomGridView; AOperationIndex: Integer): Boolean; override;
  end;

implementation

uses
  cxGridTableView, cxGraphics;

//TODO move SHow/Hide to public

{ TcxGridTableViewOperationHelper }
procedure TcxGridTableViewOperationHelper.RegisterOperations;
begin
  inherited RegisterOperations;
  RegisterOperation(GROP_SHOWCOLUMNCUSTOMIZING, DoShowColumnsCustomizing);
  RegisterOperation(GROP_SHOWGROUPINGPANEL, DoShowGroupingPanel);
  RegisterOperation(GROP_SHOWSUMMARYFOOTER, DoShowSummaryFooter);
  RegisterOperation(GROP_SHOWGRID, DoShowGrid);
  RegisterOperation(GROP_COLUMNAUTOWIDTH, DoColumnAutoWidth);
  RegisterOperation(GROP_SHOWPREVIEW, DoShowPreview);
  RegisterOperation(GROP_SHOWHEADERS, DoShowHeaders);
  RegisterOperation(GROP_SHOWCOLUMNCUSTOMIZING, DoShowColumnCustomize);
  RegisterOperation(GROP_INVERTSELECT, DoInvertSelect);
  RegisterOperation(GROP_SHOWINDICATOR, DoShowIndicator);
end;

procedure TcxGridTableViewOperationHelper.DoShowColumnsCustomizing(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    (AParameter.View as TcxGridTableView).Controller.Customization := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, (AParameter.View as TcxGridTableView).Controller.Customization);
end;

procedure TcxGridTableViewOperationHelper.DoShowGroupingPanel(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    (AParameter.View as TcxGridTableView).OptionsView.GroupByBox := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, (AParameter.View as TcxGridTableView).OptionsView.GroupByBox);
end;

procedure TcxGridTableViewOperationHelper.DoShowSummaryFooter(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    (AParameter.View as TcxGridTableView).OptionsView.Footer := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, (AParameter.View as TcxGridTableView).OptionsView.Footer);
end;

procedure TcxGridTableViewOperationHelper.DoShowGrid(
  const AParameter: TcxCustomGridOperationHelperParameters);
const
  GridLinesToBool: Array[TcxGridLines] of Boolean = (True, False, False, False);
  BoolToGridLines: Array[Boolean] of TcxGridLines = (glNone, glBoth);
begin
  if AParameter.IsPerform then
    (AParameter.View as TcxGridTableView).OptionsView.GridLines := BoolToGridLines[GetShowProperty(AParameter)]
  else SetShowProperty(AParameter, GridLinesToBool[(AParameter.View as TcxGridTableView).OptionsView.GridLines]);
end;

procedure TcxGridTableViewOperationHelper.DoColumnAutoWidth(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    (AParameter.View as TcxGridTableView).OptionsView.ColumnAutoWidth := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, (AParameter.View as TcxGridTableView).OptionsView.ColumnAutoWidth);
end;

procedure TcxGridTableViewOperationHelper.DoShowPreview(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    (AParameter.View as TcxGridTableView).Preview.Visible := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, (AParameter.View as TcxGridTableView).Preview.Visible);
end;

procedure TcxGridTableViewOperationHelper.DoShowHeaders(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    (AParameter.View as TcxGridTableView).OptionsView.Header := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, (AParameter.View as TcxGridTableView).OptionsView.Header);
end;

class function TcxGridTableViewOperationHelper.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridTableView;
end;

function TcxGridTableViewOperationHelper.IsOperationEnabled(AView: TcxCustomGridView;
  AOperationIndex: Integer): Boolean;
begin
  case AOperationIndex of
    GROP_SHOWINDICATOR:
      Result := not (AView as TcxGridTableView).ViewInfo.IndicatorViewInfo.AlwaysVisible;
    GROP_SHOWPREVIEW:
      Result := (AView as TcxGridTableView).Preview.Column <> nil;
    else
      Result := inherited IsOperationEnabled(AView, AOperationIndex);
  end;
end;

procedure TcxGridTableViewOperationHelper.DoShowColumnCustomize(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    TcxGridTableView(AParameter.View).Controller.Customization :=
      TcxShowingGridOperationHelperParameters(AParameter).Showing
  else SetShowProperty(AParameter,
    TcxGridTableView(AParameter.View).Controller.Customization);
end;

procedure TcxGridTableViewOperationHelper.DoInvertSelect(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    TcxGridTableView(AParameter.View).OptionsSelection.InvertSelect :=
      TcxShowingGridOperationHelperParameters(AParameter).Showing
  else SetShowProperty(AParameter,
    TcxGridTableView(AParameter.View).OptionsSelection.InvertSelect);
end;

procedure TcxGridTableViewOperationHelper.DoShowIndicator(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    TcxGridTableView(AParameter.View).OptionsView.Indicator :=
      TcxShowingGridOperationHelperParameters(AParameter).Showing
  else SetShowProperty(AParameter,
    TcxGridTableView(AParameter.View).OptionsView.Indicator);
end;

initialization
  RegisterGridViewOperationHelper(TcxGridTableViewOperationHelper);

end.
