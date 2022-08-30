{***************************************************************************}
{ TAdvWordCloudDropDown component                                           }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}


unit AdvWordCloudDropDown;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, AdvWordCloud, AdvDropDown;

type


  TAdvWordCloudDropDown = class(TAdvDropDown)
  private
    FWordCloud: TAdvWordCloud;
    function GetWords: TAdvWords;
    procedure SetWords(const Value: TAdvWords);
    function GetWordAppearance: TAppearance;
    procedure SetWordAppearance(const Value: TAppearance);
  protected
    procedure WordClicked(Sender: TObject; Index: integer);
    procedure BeforeDropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property WordAppearance: TAppearance read GetWordAppearance write SetWordAppearance;
    property Words: TAdvWords read GetWords write SetWords;
  end;

  TAdvWordCategoryCloudDropDown = class(TAdvDropDown)
  private
    FWordCategoryCloud: TAdvWordCategoryCloud;
    function GetWordCategories: TWordCategories;
    procedure SetWordCategories(const Value: TWordCategories);
    function GetCategoryHeader: TCategoryHeader;
    procedure SetCategoryHeader(const Value: TCategoryHeader);
  protected
    procedure WordClicked(Sender: TObject; CategoryIndex, Index: integer);
    procedure BeforeDropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CategoryHeader: TCategoryHeader read GetCategoryHeader write SetCategoryHeader;
    property Categories: TWordCategories read GetWordCategories write SetWordCategories;
  end;


implementation



{ TAdvWordCloudDropDown }

procedure TAdvWordCloudDropDown.BeforeDropDown;
begin
  inherited;
  FWordCloud.Images := Images;
end;

constructor TAdvWordCloudDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FWordCloud := TAdvWordCloud.Create(Self);
  FWordCloud.OnWordClick := WordClicked;
  Control := FWordCloud;
end;

destructor TAdvWordCloudDropDown.Destroy;
begin
  FWordCloud.Free;
  inherited;
end;

function TAdvWordCloudDropDown.GetWordAppearance: TAppearance;
begin
  Result := nil;
  if Assigned(FWordCloud) then
    Result := FWordCloud.Appearance;
end;

function TAdvWordCloudDropDown.GetWords: TAdvWords;
begin
  Result := nil;
  if Assigned(FWordCloud) then
    Result := FWordCloud.Words;
end;

procedure TAdvWordCloudDropDown.SetWordAppearance(const Value: TAppearance);
begin
  if Assigned(FWordCloud) then
    FWordCloud.Appearance.Assign(Value);
end;

procedure TAdvWordCloudDropDown.SetWords(const Value: TAdvWords);
begin
  if Assigned(FWordCloud) then
    FWordCloud.Words.Assign(Value);
end;

procedure TAdvWordCloudDropDown.WordClicked(Sender: TObject; Index: integer);
begin
  if Words[Index].Value <> '' then
    Text := Words[Index].Value
  else
    Text := Words[Index].DisplayText;

  HideDropDown(false);
end;

{ TAdvWordCategoryCloudDropDown }

procedure TAdvWordCategoryCloudDropDown.BeforeDropDown;
begin
  inherited;
  FWordCategoryCloud.Images := Images;
end;

constructor TAdvWordCategoryCloudDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FWordCategoryCloud := TAdvWordCategoryCloud.Create(Self);
  FWordCategoryCloud.OnWordClick := WordClicked;
  Control := FWordCategoryCloud;
end;

destructor TAdvWordCategoryCloudDropDown.Destroy;
begin
  FWordCategoryCloud.Free;
  inherited;
end;

function TAdvWordCategoryCloudDropDown.GetCategoryHeader: TCategoryHeader;
begin
  Result := nil;
  if Assigned(FWordCategoryCloud) then
    Result := FWordCategoryCloud.Header;
end;

function TAdvWordCategoryCloudDropDown.GetWordCategories: TWordCategories;
begin
  Result := nil;
  if Assigned(FWordCategoryCloud) then
    Result := FWordCategoryCloud.Categories;
end;

procedure TAdvWordCategoryCloudDropDown.SetCategoryHeader(
  const Value: TCategoryHeader);
begin
  if Assigned(FWordCategoryCloud) then
    FWordCategoryCloud.Header.Assign(Value);
end;

procedure TAdvWordCategoryCloudDropDown.SetWordCategories(
  const Value: TWordCategories);
begin
  if Assigned(FWordCategoryCloud) then
    FWordCategoryCloud.Categories := Value
end;

procedure TAdvWordCategoryCloudDropDown.WordClicked(Sender: TObject;
  CategoryIndex, Index: integer);
begin
  if Categories[CategoryIndex].Words[Index].Value <> '' then
    Text := Categories[CategoryIndex].Words[Index].Value
  else
    Text := Categories[CategoryIndex].Words[Index].DisplayText;

  HideDropDown(false);

end;

end.
