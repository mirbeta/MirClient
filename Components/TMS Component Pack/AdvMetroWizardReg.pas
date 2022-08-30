{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright � 2012                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage caused by the use of this code.                 }
{ The licensed user can use the source code royalty free for building any }
{ compiled application. The complete source code remains property of the  }
{ author and may not be distributed, published, given or sold in any form }
{ as such. No parts of the source code can be included in any other       }
{ component or application without                                        }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvMetroWizardReg;

interface

uses
  AdvMetroWizard, Classes, DesignIntf, ToolsAPI, DesignEditors, AdvMetroForm;

procedure Register;

implementation

procedure Register;
begin
  RegisterPackageWizard(TMetroFrmWizard.Create);
  RegisterPackageWizard(TMetroApplicationWizard.Create);
  RegisterCustomModule(TAdvMetroForm, TCustomModule);

end;

end.
