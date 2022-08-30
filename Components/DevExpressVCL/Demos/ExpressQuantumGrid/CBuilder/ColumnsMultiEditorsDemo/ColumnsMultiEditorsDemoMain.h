//---------------------------------------------------------------------------

#ifndef ColumnsMultiEditorsDemoMainH
#define ColumnsMultiEditorsDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "ColumnsMultiEditorsDemoDS.h"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
//---------------------------------------------------------------------------
class TColumnsMultiEditorsDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
   TcxGrid *Grid;
  TcxGridTableView *tvSkills;
  TcxGridColumn *clnName;
  TcxGridColumn *clnSkill;
  TcxGridColumn *clnGrade;
  TcxGridLevel *lvSkills;
  TMenuItem *miOptions;
  TMenuItem *miEditButtons;
  TcxEditRepository *EditRepository;
  TcxEditRepositoryImageComboBoxItem *ImageComboLanguages;
  TcxEditRepositoryImageComboBoxItem *ImageComboCommunication;
  TcxEditRepositorySpinItem *SpinItemYears;
  TcxEditRepositoryDateItem *DateItemStartWorkFrom;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall clnGradeGetProperties(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord,
          TcxCustomEditProperties *&AProperties);
  void __fastcall miEditButtonsAlwaysClick(TObject *Sender);
  void __fastcall miEditButtonsFocusedRecordClick(TObject *Sender);
  void __fastcall miEditButtonsNeverClick(TObject *Sender);
  void __fastcall DateItemStartWorkFromPropertiesGetDayOfWeekState(TObject *Sender, TDay ADayOfWeek,
	TCustomDrawState AState, TFont *AFont, TColor &ABackgroundColor);
private:  // User declarations
  TSkillDataSource* SkillDataSource;
public:   // User declarations
  __fastcall TColumnsMultiEditorsDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TColumnsMultiEditorsDemoMainForm *ColumnsMultiEditorsDemoMainForm;
//---------------------------------------------------------------------------
#endif
