{****************************************************************}
{ TWebUpdate & TWebUpdate Wizard component                       }
{ for Delphi & C++Builder                                        }
{                                                                }
{ written by                                                     }
{   TMS Software                                                 }
{   copyright © 1998-2010                                        }
{   Email : info@tmssoftware.com                                 }
{   Web : http://www.tmssoftware.com                             }
{****************************************************************}
unit wupdreg;

{$I TMSDEFS.INC}

interface

uses
  Classes, WUpdate, WUpdateWiz
  {$IFDEF DELPHI2006_LVL}
  , WUpdateLanguagesU;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  , WUpdateLanguages;
  {$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Web',[TWebUpdate
  , TWebUpdateWizard
  , TWebUpdateWizardDutch
  , TWebUpdateWizardEnglish
  , TWebUpdateWizardFrench
  , TWebUpdateWizardGerman
  , TWebUpdateWizardPortugese
  , TWebUpdateWizardSpanish
  , TWebUpdateWizardDanish
  , TWebUpdateWizardItalian
  , TWebUpdateWizardNorwegian
  , TWebUpdateWizardHungarian
  , TWebUpdateWizardSwedish
  , TWebUpdateWizardCzech
  , TWebUpdateWizardPolish
  , TWebUpdateWizardCatalan
  {$IFDEF DELPHI_UNICODE}
  , TWebUpdateWizardTurkish
  , TWebUpdateWizardGreek
  {$ENDIF}
  ]);
end;

end.

