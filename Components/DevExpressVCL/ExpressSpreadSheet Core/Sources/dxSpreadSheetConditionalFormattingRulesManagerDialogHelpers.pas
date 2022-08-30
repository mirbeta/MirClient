{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetConditionalFormattingRulesManagerDialogHelpers;

{$I cxVer.Inc}

interface

uses
  Types, TypInfo, Classes, Generics.Defaults, Generics.Collections, Graphics, cxGraphics,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetTypes;

type

  { IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener }

  IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener = interface
  ['{4C67D7F0-368C-4984-8B0A-ABFB41343B1B}']
    procedure NotifyDataChanged;
    procedure NotifyModified;
  end;

  { TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo }

  TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo = class
  strict private
    function GetDetails: string;
  public
    Rule: TdxSpreadSheetCustomConditionalFormattingRule;
    StopIfTrue: Boolean;
    StopIfTrueSupported: Boolean;

    constructor Create(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    procedure ApplyChanges(AIndex: Integer);
    procedure DrawPreview(ACanvas: TcxCanvas; const R: TRect);
    //
    property Details: string read GetDetails;
  end;

  { TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper }

  TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperEnumRulesProc = reference to procedure (ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo);

  TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper = class
  strict private
    FConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
    FHasChanges: Boolean;
    FListener: IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener;
    FRules: TObjectList<TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo>;
    FRulesAdded: TObjectList<TdxSpreadSheetCustomConditionalFormattingRule>;
    FRulesDeleted: TList<TdxSpreadSheetCustomConditionalFormattingRule>;

    procedure InternalDeleteRule(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    procedure SetHasChanges(const Value: Boolean);
  protected
    // History
    procedure BeginEditing; virtual;
    procedure EndEditing(ACanceled: Boolean); virtual;

    procedure DataChanged;
  public
    constructor Create(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
      AListener: IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener);
    destructor Destroy; override;
    procedure ApplyChanges;
    procedure BeforeDestruction; override;
    procedure PopulateRulesInArea(const AArea: TRect; AProc: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperEnumRulesProc);

    procedure Add(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    procedure Delete(AInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo);
    procedure MoveTo(ASource, ATarget: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo; AInsertAfter: Boolean);
    procedure Replace(ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo;
      ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    procedure SetAreas(ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo; AAreas: TdxSpreadSheetAreaList);

    property HasChanges: Boolean read FHasChanges write SetHasChanges;
    property ConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting read FConditionalFormatting;
  end;
  TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperClass = class of TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper;

implementation

uses
  dxCore, SysUtils, dxSpreadSheetUtils, dxSpreadSheetGraphics;

type
  TdxSpreadSheetCustomConditionalFormattingRuleAccess = class(TdxSpreadSheetCustomConditionalFormattingRule);

{ TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo }

constructor TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo.Create(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited Create;
  Rule := ARule;
  StopIfTrueSupported := GetPropInfo(ARule, 'StopIfTrue') <> nil;
  if StopIfTrueSupported then
    StopIfTrue := TdxSpreadSheetCustomConditionalFormattingRuleAccess(Rule).StopIfTrue;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo.ApplyChanges(AIndex: Integer);
begin
  Rule.Index := AIndex;
  if StopIfTrueSupported then
    TdxSpreadSheetCustomConditionalFormattingRuleAccess(Rule).StopIfTrue := StopIfTrue;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo.DrawPreview(ACanvas: TcxCanvas; const R: TRect);
begin
  TdxSpreadSheetCustomConditionalFormattingRuleAccess(Rule).DrawPreview(ACanvas, R);
end;

function TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo.GetDetails: string;
begin
  Result := TdxSpreadSheetCustomConditionalFormattingRuleAccess(Rule).GetDetails;
end;

{ TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper }

constructor TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.Create(
  AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
  AListener: IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener);
var
  I: Integer;
begin
  inherited Create;
  FListener := AListener;
  FConditionalFormatting := AConditionalFormatting;
  FRulesAdded := TObjectList<TdxSpreadSheetCustomConditionalFormattingRule>.Create;
  FRulesDeleted := TList<TdxSpreadSheetCustomConditionalFormattingRule>.Create;

  FRules := TObjectList<TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo>.Create;
  FRules.Capacity := ConditionalFormatting.RuleCount;
  for I := 0 to ConditionalFormatting.RuleCount - 1 do
    FRules.Add(TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo.Create(ConditionalFormatting[I]));

  BeginEditing;
end;

destructor TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.Destroy;
begin
  EndEditing(True);
  FreeAndNil(FRulesDeleted);
  FreeAndNil(FRulesAdded);
  FreeAndNil(FRules);
  inherited Destroy;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.ApplyChanges;
var
  I: Integer;
begin
  if HasChanges then
  begin
    ConditionalFormatting.BeginUpdate;
    try
      for I := 0 to FRulesDeleted.Count - 1 do
        FRulesDeleted[I].Free;
      FRulesDeleted.Clear;

      FRulesAdded.OwnsObjects := False;
      FRulesAdded.Clear;
      FRulesAdded.OwnsObjects := True;

      dxTestCheck(FRules.Count = ConditionalFormatting.RuleCount, 'SyncError');

      for I := 0 to FRules.Count - 1 do
        TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(FRules[I]).ApplyChanges(I);
    finally
      ConditionalFormatting.EndUpdate;
    end;
    EndEditing(False);
    DataChanged;
    BeginEditing;
  end;
  HasChanges := False;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.BeforeDestruction;
begin
  inherited BeforeDestruction;

  ConditionalFormatting.BeginUpdate;
  try
    FRulesDeleted.Clear;
    FRulesAdded.Clear;
    FRules.Clear;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.PopulateRulesInArea(
  const AArea: TRect; AProc: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperEnumRulesProc);
var
  AInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo;
  I: Integer;
begin
  for I := 0 to FRules.Count - 1 do
  begin
    AInfo := FRules[I];
    if AInfo.Rule.Areas.Intersects(AArea) then
      AProc(AInfo);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.Add(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule);
var
  ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo;
begin
  ARuleInfo := TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo.Create(ARule);
  FRulesAdded.Add(ARule);
  FRules.Add(ARuleInfo);
  DataChanged;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.Delete(
  AInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo);
begin
  InternalDeleteRule(AInfo.Rule);
  FRules.Remove(AInfo);
  DataChanged;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.MoveTo(
  ASource, ATarget: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo; AInsertAfter: Boolean);
var
  AIndex: Integer;
begin
  FRules.Extract(ASource);
  AIndex := FRules.IndexOf(ATarget);
  if AInsertAfter then
    Inc(AIndex);
  FRules.Insert(AIndex, ASource);
  DataChanged;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.Replace(
  ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  if ARule <> ARuleInfo.Rule then
  begin
    InternalDeleteRule(ARuleInfo.Rule);
    FRulesAdded.Add(ARule);
    ARuleInfo.Rule := ARule;
  end;
  DataChanged;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.SetAreas(
  ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo; AAreas: TdxSpreadSheetAreaList);
begin
  if FRulesAdded.Contains(ARuleInfo.Rule) then
    ARuleInfo.Rule.Areas := AAreas
  else
    Replace(ARuleInfo, ARuleInfo.Rule.Clone(AAreas));

  DataChanged;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.BeginEditing;
begin
  ConditionalFormatting.BeginEditing;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.EndEditing(ACanceled: Boolean);
begin
  ConditionalFormatting.EndEditing(ACanceled);
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.DataChanged;
begin
  HasChanges := True;
  FListener.NotifyDataChanged;
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.InternalDeleteRule(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  if FRulesAdded.Remove(ARule) < 0 then
    FRulesDeleted.Add(ARule);
end;

procedure TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper.SetHasChanges(const Value: Boolean);
begin
  if FHasChanges <> Value then
  begin
    FHasChanges := Value;
    FListener.NotifyModified;
  end;
end;

end.
