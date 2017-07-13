--assembler settings
--software
codetype = 1 --1 for text , 2 for rodata
align = 1  --binary alignment
--hardware
rm = "000"
aq= "0"
rl = "0"
R_TypeOps = {
	["ADD"]			= "0000000" .. "rs2" .. "rs1" .. "000" .. "rd" .. "0110011",
	["SUB"]			= "0100000" .. "rs2" .. "rs1" .. "000" .. "rd" .. "0110011",
	["SLL"]			= "0000000" .. "rs2" .. "rs1" .. "001" .. "rd" .. "0110011", 
	["SLT"]			= "0000000" .. "rs2" .. "rs1" .. "010" .. "rd" .. "0110011",
	["SLTU"]		= "0000000" .. "rs2" .. "rs1" .. "011" .. "rd" .. "0110011",
	["XOR"]			= "0000000" .. "rs2" .. "rs1" .. "100" .. "rd" .. "0110011",
	["SRL"]			= "0000000" .. "rs2" .. "rs1" .. "101" .. "rd" .. "0110011",
	["SRA"]			= "0100000" .. "rs2" .. "rs1" .. "101" .. "rd" .. "0110011",
	["OR"]			= "0000000" .. "rs2" .. "rs1" .. "110" .. "rd" .. "0110011",
	["AND"]			= "0000000" .. "rs2" .. "rs1" .. "111" .. "rd" .. "0110011",
	["MUL"]			= "0000001" .. "rs2" .. "rs1" .. "000" .. "rd" .. "0110011",
	["MULH"]		= "0000001" .. "rs2" .. "rs1" .. "001" .. "rd" .. "0110011",
	["MULHSU"]	= "0000001" .. "rs2" .. "rs1" .. "010" .. "rd" .. "0110011",
	["MULHU"]		= "0000001" .. "rs2" .. "rs1" .. "011" .. "rd" .. "0110011",
	["DIV"]			= "0000001" .. "rs2" .. "rs1" .. "100" .. "rd" .. "0110011",
	["DIVU"]		= "0000001" .. "rs2" .. "rs1" .. "101" .. "rd" .. "0110011",
	["REM"]			= "0000001" .. "rs2" .. "rs1" .. "110" .. "rd" .. "0110011",
	["REMU"]		= "0000001" .. "rs2" .. "rs1" .. "111" .. "rd" .. "0110011",
	["ADDW"]		= "0000000" .. "rs2" .. "rs1" .. "000" .. "rd" .. "0111011",
	["SUBW"]		= "0100000" .. "rs2" .. "rs1" .. "000" .. "rd" .. "0111011",
	["SLLW"]		= "0000000" .. "rs2" .. "rs1" .. "001" .. "rd" .. "0111011",
	["SRLW"]		= "0000000" .. "rs2" .. "rs1" .. "101" .. "rd" .. "0111011",
	["SRAW"]		= "0100000" .. "rs2" .. "rs1" .. "101" .. "rd" .. "0111011",
	["MULW"]		= "0000001" .. "rs2" .. "rs1" .. "000" .. "rd" .. "0111011",
	["DIVW"]		= "0000001" .. "rs2" .. "rs1" .. "100" .. "rd" .. "0111011",
	["DIVUW"]		= "0000001" .. "rs2" .. "rs1" .. "101" .. "rd" .. "0111011",
	["REMW"]		= "0000001" .. "rs2" .. "rs1" .. "110" .. "rd" .. "0111011",
	["REMUW"]		= "0000001" .. "rs2" .. "rs1" .. "111" .. "rd" .. "0111011",
	["FADD.S"]		= "0000000" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FSUB.S"]		= "0000100" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FMUL.S"]		= "0001000" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FDIV.S"]		= "0001100" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FSQRT.S"]		= "0101100" .. "00000" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FSGNJ.S"]		= "0010000" .. "rs2" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FSGNJN.S"]	= "0010000" .. "rs2" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FSGNJX.S"]	= "0010000" .. "rs2" .. "rs1" .. "010" .. "rd" .. "1010011",
	["FMIN.S"]		= "0010100" .. "rs2" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FMAX.S"]		= "0010100" .. "rs2" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FCVT.W.S"]	= "1100000" .. "00000" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.WU.S"]	= "1100000" .. "00001" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FMV.X.S"]		= "1110000" .. "00000" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FEQ.S"]		= "1010000" .. "rs2" .. "rs1" .. "010" .. "rd" .. "1010011",
	["FLT.S"]		= "1010000" .. "rs2" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FLE.S"]		= "1010000" .. "rs2" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FCLASS.S"]	= "1110000" .. "00000" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FCVT.S.W"]	= "1101000" .. "00000" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.S.WU"]	= "1101000" .. "00001" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FMV.S.X"]		= "1111000" .. "00000" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FCVT.L.S"]	= "1100000" .. "00010" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.LU.S"]	= "1100000" .. "00011" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.S.L"]	= "1101000" .. "00010" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.S.LU"]	= "1101000" .. "00011" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.L.D"]	= "1100001" .. "00010" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.LU.D"]	= "1100001" .. "00011" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FMV.X.D"]		= "1100001" .. "00000" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FCVT.D.L"]	= "1101001" .. "00010" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.D.LU"]	= "1101001" .. "00011" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FMV.D.X"]		= "1111001" .. "00000" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FADD.D"]		= "0000001" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FSUB.D"]		= "0000101" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FMUL.D"]		= "0001001" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FDIV.D"]		= "0001101" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FSQRT.D"]		= "0101101" .. "00000" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FSGNJ.D"]		= "0010001" .. "rs2" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FSGNJN.D"]	= "0010001" .. "rs2" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FSGNJX.D"]	= "0010001" .. "rs2" .. "rs1" .. "010" .. "rd" .. "1010011",
	["FMIN.D"]		= "0010101" .. "rs2" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FMAX.D"]		= "0010101" .. "rs2" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FCVT.S.D"]	= "0100000" .. "00001" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.D.S"]	= "0100001" .. "00000" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FEQ.D"]		= "1010001" .. "rs2" .. "rs1" .. "010" .. "rd" .. "1010011",
	["FLT.D"]		= "1010001" .. "rs2" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FLE.D"]		= "1010001" .. "rs2" .. "rs1" .. "000" .. "rd" .. "1010011",
	["FCLASS.S"]	= "1110001" .. "00000" .. "rs1" .. "001" .. "rd" .. "1010011",
	["FCVT.W.D"]	= "1100001" .. "00000" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.WU.D"]	= "1100001" .. "00001" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.D.W"]	= "1100001" .. "00000" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["FCVT.D.WU"]	= "1100001" .. "00001" .. "rs1" .. "rm" .. "rd" .. "1010011",
	["LR.W"]		= "00010" .. "aq" .. "rl" .."00000" .. "rs1" .. "010" .. "rd" .."0101111",
	["SC.W"]		= "00011" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOSWAP.W"]	= "00001" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOADD.W"]	= "00000" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOXOR.W"]	= "00100" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOAND.W"]	= "01100" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOOR.W"]		= "01000" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOMIN.W"]	= "10000" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOMAX.W"]	= "10100" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOMINU.W"]	= "11000" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111",
	["AMOMAXU.W"]	= "11100" .. "aq" .. "rl" .."rs2" .. "rs1" .. "010" .. "rd" .."0101111"

}

R4_TypeOps = {
	["FMADD.S"]  	= "rs3" .. "00" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1000011",
	["FMSUB.S"]		= "rs3" .. "00" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1000111",
	["FNMSUB.S"]	= "rs3" .. "00" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1001011",
	["FNMADD.S"]	= "rs3" .. "00" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1001111",
	["FMADD.D"]  	= "rs3" .. "01" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1000011",
	["FMSUB.D"]		= "rs3" .. "01" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1000111",
	["FNMSUB.D"]	= "rs3" .. "01" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1001011",
	["FNMADD.D"]	= "rs3" .. "01" .. "rs2" .. "rs1" .. "rm" .. "rd" .. "1001111"

}



U_TypeOps = {
	LUI			= "imm[31:12]" .. "rd" .. "0110111",
	AUIPC		= "imm[31:12]" .. "rd" .. "0010111"
}

UJ_TypeOps = {
	JAL			= "imm" .. "rd" .. "1101111"
}

SB_TypeOps = {
	BEQ			= "imm[10:5]" .. "rs2" .. "rs1" .. "000" .. "imm[4:1]" .. "1100011",
	BNE			= "imm[10:5]" .. "rs2" .. "rs1" .. "001" .. "imm[4:1]" .. "1100011",
	BLT			= "imm[10:5]" .. "rs2" .. "rs1" .. "100" .. "imm[4:1]" .. "1100011",
	BGE			= "imm[10:5]" .. "rs2" .. "rs1" .. "101" .. "imm[4:1]" .. "1100011",
	BLTU		= "imm[10:5]" .. "rs2" .. "rs1" .. "110" .. "imm[4:1]" .. "1100011",
	BGEU		= "imm[10:5]" .. "rs2" .. "rs1" .. "111" .. "imm[4:1]" .. "1100011"
}

I_TypeOps = {
	JALR		= "imm[11:0]" .. "rs1" .. "000" .. "rd" .. "1100111",
	LB			= "imm[11:0]" .. "rs1" .. "000" .. "rd" .. "0000011",
	LH			= "imm[11:0]" .. "rs1" .. "001" .. "rd" .. "0000011",
	LW			= "imm[11:0]" .. "rs1" .. "010" .. "rd" .. "0000011",
	LD			= "imm[11:0]" .. "rs1" .. "011" .. "rd" .. "0000011",
	LBU			= "imm[11:0]" .. "rs1" .. "100" .. "rd" .. "0000011",
	LHU			= "imm[11:0]" .. "rs1" .. "101" .. "rd" .. "0000011",
	LWU			= "imm[11:0]" .. "rs1" .. "110" .. "rd" .. "0000011",
	ADDI		= "imm[11:0]" .. "rs1" .. "000" .. "rd" .. "0010011",
  	ADDIW		= "imm[11:0]" .. "rs1" .. "000" .. "rd" .. "0011011",
	SLTI		= "imm[11:0]" .. "rs1" .. "010" .. "rd" .. "0010011",
	SLTIU		= "imm[11:0]" .. "rs1" .. "011" .. "rd" .. "0010011",
	XORI		= "imm[11:0]" .. "rs1" .. "100" .. "rd" .. "0010011",
	ORI			= "imm[11:0]" .. "rs1" .. "110" .. "rd" .. "0010011",
	ANDI		= "imm[11:0]" .. "rs1" .. "111" .. "rd" .. "0010011",
	LWU         = "imm[11:0]" .. "rs1" .. "110" .. "rd" .. "0000011",
	FLW			= "imm[11:0]" .. "rs1" .. "010" .. "rd" .. "0000111",
	FLD			= "imm[11:0]" .. "rs1" .. "011" .. "rd" .. "0000111",
	SLLI		= "000000" .. "imm[5:0]" .. "rs1" .. "001" .. "rd" .. "0010011",
	SRLI		= "000000" .. "imm[5:0]" .. "rs1" .. "101" .. "rd" .. "0010011",
	SRAI		= "010000" .. "imm[5:0]" .. "rs1" .. "101" .. "rd" .. "0010011",
	ECALL		= "0000000000000000000000000" .. "1110011",
	EBREAK		= "0000000000010000000000000" .. "1110011",
	URET		= "000000000010" .. "00000" .. "000" .. "00000" .. "1110011",
	SRET		= "000100000010" .. "00000" .. "000" .. "00000" .. "1110011",
	HRET		= "001000000010" .. "00000" .. "000" .. "00000" .. "1110011",
	MRET		= "001100000010" .. "00000" .. "000" .. "00000" .. "1110011",
	WFI			= "000100000101" .. "00000" .. "000" .. "00000" .. "1110011",
  SFENCEVM	= "000100000100" .. "rs1" .. "000" .. "00000" .. "1110011",
	CSRRW		= "csr" .. "rs1" .. "001" .. "rd" .. "1110011",
	CSRRS		= "csr" .. "rs1" .. "010" .. "rd" .. "1110011",
	CSRRC		= "csr" .. "rs1" .. "011" .. "rd" .. "1110011",
	CSRRWI		= "csr" .. "imm[4:0]" .. "101" .. "rd" .. "1110011",
	CSRRSI		= "csr" .. "imm[4:0]" .. "110" .. "rd" .. "1110011",
	CSRRCI		= "csr" .. "imm[4:0]" .. "111" .. "rd" .. "1110011",
	SLLIW 		= "0000000" .. "imm[4:0]" .. "rs1" .. "001" .. "rd" .. "0011011",
	SRLIW 		= "0000000" .. "imm[4:0]" .. "rs1" .. "101" .. "rd" .. "0011011",
	SRAIW 		= "0100000" .. "imm[4:0]" .. "rs1" .. "101" .. "rd" .. "0011011"

}

S_TypeOps = {
	SB			= "imm[11:5]" .. "rs2" .. "rs1" .. "000" .. "imm[4:0]" .. "0100011",
	SH			= "imm[11:5]" .. "rs2" .. "rs1" .. "001" .. "imm[4:0]" .. "0100011",
	SW			= "imm[11:5]" .. "rs2" .. "rs1" .. "010" .. "imm[4:0]" .. "0100011",
	SD			= "imm[11:5]" .. "rs2" .. "rs1" .. "011" .. "imm[4:0]" .. "0100011",
	FSW			= "imm[11:5]" .. "rs2" .. "rs1" .. "010" .. "imm[4:0]" .. "0100111",
	FSD			= "imm[11:5]" .. "rs2" .. "rs1" .. "011" .. "imm[4:0]" .. "0100111",
}



csrcodes = {
	stvec		= "‭000100000101‬",
	sscratch 	= "‭000101000000‬",
	ustatus 	= "000000000000",
	mtvec		= "‭001100000101‬",
	msscratch	= "001101000000‬"
}


pseudoinstructions0 = {}
pseudoinstructions1 = {}
pseudoinstructions2 = {}
pseudoinstructions3 = {}
pseudoinstructions0.RET = {"JALR x0,x1,0"}
pseudoinstructions0.NOP = {"ADDI x0,x0,0"}
pseudoinstructions1.PUSH = {"SW rs1,0(x2)", "ADDI X2,X2,4"}
pseudoinstructions1.POP = {"ADDI x2,x2,-4","LW rs1,0(x2)"}
--pseudoinstructions1.CALL = {"AUIPC x6, rs1","JALR x1, x6, rs1"}
pseudoinstructions1.CALL = {"AUIPC x6, 0","JALR x1,x6, rs1"}
pseudoinstructions1.TAIL = {"AUIPC x6, 0","JALR x0, x6, rs1"}
pseudoinstructions1.J	= {"JAL x0, rs1"}
pseudoinstructions1.JR	= {"JALR x0, rs1, 0"}
--loadimmediate = {"LUI rs1, rs2","ADDI x6,x0,rs2","SLLI x6,x6,1","ADD rs1,rs1,x6"}
--loadimmediate = {"LUI rs1, rs2","ADD rs1,rs1,rs2"}
loadimmediate = {"LUI rs1,rs2","ADDI rs1,rs1,rs2"}
loadaddress = {"AUIPC rs1 , rs2","ADDI rs1 , rs1 , rs2"}
pseudoinstructions2 ={
["BEQZ"] 	= "BEQ rs1, x0, rs2",
["BNEZ"]	= "BNE rs1, x0, rs2",
["BLEZ"]	= "BGE x0, rs1, rs2",
["BGEZ" ]	= "BGE rs1, x0, rs2",
["BLTZ"]	= "BLT rs1, x0, rs2",
["BGTZ"]    = "BLT x0, rs1, rs2",
["fmv.s"]   ="fsgnj.s rs1, rs2, rs2",
["fabs.s"]  ="fsgnjx.s rs1, rs2, rs2",
["fneg.s"]  ="fsgnjn.s rs1, rs2, rs2",
["fmv.d"]   ="fsgnj.d rs1, rs2, rs2",
["fabs.d"]  ="fsgnjx.d rs1, rs2, rs2",
["fneg.d"]  ="fsgnjn.d rs1, rs2, rs2",
["MV"]		="ADDI rs1, rs2, 0",
["NOT"]		="XORI rs1, rs2, -1",
["NEG"]		="SUB rs1,x0,rs2",
["NEGW"]	="SUBW rs1,x0,rs2",
["SEXT.W"]="ADDIW rs1,rs2,0",
["SEQZ"]	="SLTIU rs1,rs2,1",
["SNEZ"]	="SLTU rs1,x0,rs2",
["SLTZ"]	="SLT rs1, rs2 ,x0",
["SGTZ"]	="SLT rs1,x0,rs2",
["LI"]		=loadimmediate ,
["LA"]    =loadaddress
}

pseudoinstructions3 = {
  ["BGT"] = "BLT rs2,rs1,rs3",
  ["BGT"] = "BGE rs2,rs1,rs3",
  ["BGT"] = "BLTU rs2,rs1,rs3",
  ["BGT"] = "BGEU rs2,rs1,rs3",
}

typeslist ={R_TypeOps, U_TypeOps, UJ_TypeOps, SB_TypeOps, I_TypeOps, S_TypeOps,R4_TypeOps}

regnames = {
	["zero"] = "x0",
	["ra"] = "x1",
	["sp"] = "x2",
	["gp"] = "x3",
	["tp"] = "x4",
	["t0"] = "x5",
	["t1"] = "x6",
	["t2"] = "x7",
	["s0"] = "x8",
	["fb"] = "x8",
	["s1"] = "x9",
	["a0"] = "x10",
	["a1"] = "x11",
	["a2"] = "x12",
	["a3"] = "x13",
	["a4"] = "x14",
	["a5"] = "x15",
	["a6"] = "x16",
	["a7"] = "x17",
	["s2"] = "x18",
	["s3"] = "x19",
	["s4"] = "x20",
	["s5"] = "x21",
	["s6"] = "x22",
	["s7"] = "x23",
	["s8"] = "x24",
	["s9"] = "x25",
	["s10"] = "x26",
	["s11"] = "x27",
	["t3"] = "x28",
	["t4"] = "x29",
	["t5"] = "x30",
	["t6"] = "x31"
}

dataflag = false

function replace_char(pos, str, r)
    return str:sub(1, pos-1) .. r .. str:sub(pos+1)
end

function trim(s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end


function insertins(counter ,limit ,instructioncode ,outputfile)
	if counter < limit then 
		for i =1,(limit-counter) do
			for i=25, 0,-8 do 
				outputfile:write(instructioncode:sub(i,i+7),"\n") 
			end
		end
	end  
end


function extractdata(line)
	local out = ""
	if line:match("^.LC") ~= nil then 
	codetype = 2
	return line
	end
	if (line:match(".string")) and (dataflag == true) ~= nil then
		temptable = {}
		out = {}
		temp = line:sub(line:find(".string")+7,#line)
		temp = trim(temp)
		temp = temp:gsub("\"","")
		for i = 1, #temp do
			a  = temp:sub(i,i):byte()
			a = to_binary(a,8,true)
			temptable[#temptable+1] = a
		end
		temptable[#temptable+1] = "00000000"
		addition =(math.ceil(#temptable/4)*4-#temptable)
		print(addition)
		for i = 1,(addition) do
			temptable[#temptable+1] = "00000000"
		end
		for i = 1,#temptable-3,4 do
			out[#out+1]=table.concat( temptable, "", i, i+3 )
		end
		for i,n in pairs(out) do
			instructions[#instructions+1] = n
			typetable[#typetable+1] = codetype
			counter = counter +1
		end
		dataflag = false
		line = ""
		return line
	end
	if (line:match(".word")) and (dataflag == true) ~= nil then
		temptable = {}
		out = {}
		temp = line:sub(line:find(".string")+7,#line)
		temp = trim(temp)
		temp = temp:gsub("\"","")
		for i = 1, #temp do
			a  = temp:sub(i,i):byte()
			a = to_binary(a,8,true)
			temptable[#temptable+1] = a
		end
		temptable[#temptable+1] = "00000000"
		addition =(math.ceil(#temptable/4)*4-#temptable)
		print(addition)
		for i = 1,(addition) do
			temptable[#temptable+1] = "00000000"
		end
		for i = 1,#temptable-3,4 do
			out[#out+1]=table.concat( temptable, "", i, i+3 )
		end
		for i,n in pairs(out) do
			instructions[#instructions+1] = n
			typetable[#typetable+1] = codetype
			counter = counter +1
		end
		dataflag = false
		line = ""
		return line
	end
	return line
end


function extractoptions(line)
-- type, alignment
	if(line:find(".text") ~= nil) then
		codetype = 1
		line =""
	end
	if(line:find(".rodata") ~= nil) then
		codetype = 2
		line =""
	end
	if(line:find(".file") ~= nil) then
		temp = line:sub(line:find(".string")+5,#line)
    	temp = trim(temp)
    	temp = temp:gsub("\"","")
		temp:gsub(".s","")
		outputfilename = temp .. ".txt"
		line =""
	end
	if(line:find(".align") ~= nil) then 
		align =tonumber(line:sub(line:find(".align")+7,#line))
		line =""
	end
	if(line:find(".globl") ~= nil)then 
		line =""
	end
	if(line:find(".type") ~= nil)then
		line =""
	end
	if(line:find(".option") ~= nil)then
		line =""
	end
	if(line:find(".section") ~= nil)then
		line =""
	end
	if(line:find(".ident") ~= nil)then
		line =""
	end
	if(line:find(".size") ~= nil)then
		line =""
	end
	return line
end

function removecomments(line)
	if (line:find(commentchar) ~= nil) then
		commentstart = line:find(commentchar)
		line = line:sub(1,commentstart-1)
		possibleins  = line
		possibleins = trim(possibleins)
		if  #possibleins~=0 then
			if (isins(possibleins)  ) then
				--counter = counter +1
				--instructions[#instructions+1] = possibleins
				line = possibleins
			else
				print("ERROR: you wrote something not instruction before the ; ")
				print("at line " .. counter)
				errors[#errors+1] = "you wrote something not instruction before the ; at line  " .. counter
			end
		end
	end
	return line
end

function replaceregname(line)
	for key,value in pairs(regnames) do
		exp1="%s+".. key .."+%s+"
		exp2="%s*".. key .."%s*[,]"
		exp3 = "[,]%s*".. key .."$"
		exp4="%s+a5%s*[,]"
		if (line:find(key) ~= nil )then
			line =line:gsub(key,value)
			print("replaced " .. key .. " with " .. value.. "----------------" )
		end
	end
	return line 
end


function lohi(line)
  if line:find("hi%(") ~= nil then
  	print("line before replacing hi " ..line .."---------------")
    exp = "%hi%(([-]?%d*)"
    imm= line:match(exp)
    print("imm detedted in ")
    print("imm before replace  ".. imm)
    imm = tonumber(imm)
    imm,overflow = to_binary(imm, 32, true)
    imm2 = tonumber(imm:sub(1,20) .. "000000000000",2)
    pos = line:find("%(")
    line2 = line:sub(1,pos-3)
    line = line2.. " "..imm2
    print("replaced hi ")
    print("replaced ".. imm .." with " .. imm2)
    print(line)
    return line
  end
  if line:find("lo%(") ~= nil then
  	print("line before replacing lo " ..line .."---------------")
    exp = "%lo%(([-]?%d*)"
    imm= line:match(exp)
    imm = tonumber(imm)
    imm,overflow = to_binary(imm, 32, true)
    imm2 = tonumber("00000000000000000000" .. imm:sub(21,32),2)
    pos = line:find("%(")
    line2 = line:sub(1,pos-3)
    line = line2 .. " "..imm2
    print("replaced lo ")
    print("replaced ".. imm .." with " .. imm2)
    print(line)
    return line
  end
  return line
end

function storelabels(line) 
  if (line:find(":") ~= nil) then
    labellength = line:find(":")
    label  = line:sub(1, labellength - 1)
    label = label:gsub(' ', "")
    if label ~= "" then 
      if (tonumber(label:sub(1,1)) == nil) or (label:find(",") ~= nil) then
        --if (counter == 0) then
        --  labels[label] = 0
        --else
        --  labels[label] = (counter+1)*4
        --end
		labels[label] = (counter+1)*4
      else
        errors[#errors+1] = "you entered a label that starts with a number or has a , in it at line " .. counter
      end
      possibleins = line:sub(labellength+1,#line)
      line = line:sub(labellength+1,#line)
      possibleins = trim(possibleins)
      if #possibleins~=0 then
        --if (isins(possibleins) ) then
        --counter = counter+1
        --instructions[#instructions+1] = possibleins
        line = possibleins
        --else
        --	print(" ERROR : you entered something not instruction after : ")
        --	print(possibleins)
        --	print("at line " .. counter)
        --	errors[#errors+1] = "you entered something not instruction after :at line " .. counter
        --	errors[#errors+1]  = possibleins
        --end
      end
    else
      errors[#errors+1] = "zero length label at line " .. counter
    end
  end
  return line
end

function writelittleendian(binarycodearray,outputfile)
	for i,code in ipairs(binarycodearray) do
		for i=25, 0,-8 do 
			outputfile:write(code:sub(i,i+7),"\n") 
		end
	end
end

function  extractins(asmfile)
	line = asmfile:read("*line")
	while (line ~= nil  ) do
		line = extractoptions(line)
		line = removecomments(line)
		line = replaceregname(line)
		line = trim(line)
		line = replacepseudo(line)
		line = extractdata(line)
		line = replaceregname(line)
		line = trim(line)
		line = storelabels(line)
		line = trim(line)
		--line = replacepseudo(line)
		--line = lohi(line)
		if (#line ~= 0) then
			print("checking if it is an instruction to add to table " .. line)
			if isins(line) then
				instructions[#instructions+1] = line
				typetable[#typetable+1] = codetype
				counter = counter +1
				print("added to instructions " .. line)
			else
				print(line)
				errors[#errors+1] = "ERROR: you entered garbage instead of instructions  at line " .. counter 
			end
		end
		line = asmfile:read("*line")
	end
	return instructions ,labels
end
dic = {}
function replacelabels(instructions, labels)
	for i, instruction in ipairs(instructions) do
		if (instruction ~=nil) then
			--print("instruction is not nil -------------------------------------------")
			for key,value in pairs(labels) do
				if (instruction:find(key) ~= nil  ) then
					print(key .. "------------------------------------------ ") 
					print(instruction)
					print("label value " .. value)
					print("current instruction address " .. (i-1)*4)
					if (instruction:find("JALR") ~= nil ) then
						instruction = instruction:gsub(key,value-((i-1)*4))
						--instruction = instruction:gsub(key,value-(i*4))
						print("--------------------------------------------")
					elseif ((instruction:find("LUI") ~= nil ) or (instruction:find("lui") ~= nil )) then
						instruction = instruction:gsub(key,value)
					elseif ((instruction:find("ADDI") ~= nil) or (instruction:find("addi") ~= nil) ) then
						instruction = instruction:gsub(key,value)
					else
						instruction = instruction:gsub(key,value-(i*4))
						print("replace label with ".. value-(i*4))
					end
					instructions[i] = instruction
					print(instruction)
				end
			end
			--instruction = instruction:gsub("%%","")
			instruction = lohi(instruction)
			instructions[i] = instruction
		end
	end
	return instructions
end
-- check if it's an instruction
function isins(suspect)
	out = true
	if (#suspect ~= 0) then
		for i,type in pairs(typeslist) do
			for j,ins in pairs(type) do
				if(suspect:find(j) ~= nil or suspect:find(j:lower()) ~= nil) then
					out = true
				end
			end
		end
		for i,ins in pairs(pseudoinstructions1) do
			temph = i .. '%s'
			templ = i:lower() .. '%s'
			if(suspect:find(temph) ~= nil or suspect:find(templ) ~= nil) then
				out = true
			end
		end
		for i,ins in pairs(pseudoinstructions2) do
			temph = i .. '%s'
			templ = i:lower() .. '%s'
			if(suspect:find(temph) ~= nil or suspect:find(templ) ~= nil) then
				out = true
			end
		end
	else
		out =false
	end
	return out
end


function replacepseudo(line)
	if (line ~= nil) then
		for i ,replacement in pairs(pseudoinstructions0) do
			if (line:find(i) ~= nil or line:find(i:lower()) ~= nil ) then
				if type(replacement) == "table" then
					for j ,replacment2 in ipairs(replacement) do
						print("replaced RET with " .. replacment2)
						instructions[#instructions+1] = replacment2
						typetable[#typetable+1] = codetype
						counter = counter +1
						line = ""
						return line
					end
				elseif type(replacment) == "table" then
					print("replaced RET with " .. replacment2)
					instructions[#instructions+1] = replacment2
					typetable[#typetable+1] = codetype
					counter = counter +1
					line = ""
					return line
				end
			end
		end
		for i ,replacment in pairs(pseudoinstructions1) do
			temph ='^' .. i .. '%s'
			templ ='^' .. i:lower() .. '%s'
			if (line:find(temph) ~= nil or line:find(templ) ~= nil ) then
				print("found pseudo line  " .. line .." at " .. counter)
				oplength = line:find(" ")
				if oplength ~= nil then
					op = line:sub(1, oplength - 1)
					op = op:upper()
				end
				rs1limit = #line
				if (rs1limit ~= nil and oplength ~=nil ) then
					--errors[#errors+1] = " error at pseudo instruction at line " .. counter
					--print(" error at pseudo instruction at line " .. counter)
					rs1 = line:sub(oplength,rs1limit)
					rs1 = line:sub(oplength,rs1limit)
					rs1 = rs1:gsub(' ', "")
					rs1 = rs1:gsub(',', "")
				end
				if type(replacment) == "string" then
					if rs1 ~= nil then
						replacment = replacment:gsub("rs1", rs1)
					end
					instructions[#instructions+1] = replacment
					typetable[#typetable+1] = codetype
					counter = counter +1
					line = ""
				elseif type(replacment) == "table" then
					for j ,replacment2 in ipairs(replacment) do
						if rs1 ~= nil then
							replacment2 = replacment2:gsub("rs1", rs1)
						end
						instructions[#instructions+1] = replacment2
						typetable[#typetable+1] = codetype
						counter = counter +1
						line = ""
					end
				end
				return line
			end
		end
		for i ,replacment in pairs(pseudoinstructions2) do
			temph ='^' .. i .. '%s'
			templ ='^' .. i:lower() .. '%s'
			if (line:find(temph) ~= nil or line:find(templ) ~= nil) then
				print("found pseudo at " .. counter)
				oplength = line:find(" ")
				if oplength ~= nil then
					op = line:sub(1, oplength - 1)
					op = op:upper()
				end
				rs1 = ""
				rs2 = ""
				rs1limit = line:find(",", oplength)
				if rs1limit == nil  then
					errors[#errors+1] = " error at pseudo instruction at line " .. counter
				end
				if (rs1limit ~= nil and oplength ~=nil ) then
					rs1 = line:sub(oplength,rs1limit-1)
					rs1 = rs1:gsub(' ', "")
					rs1 = rs1:gsub(',', "")
				end
				rs2limit = #line
				if (rs2limit ~= nil and oplength ~=nil ) then
					rs2 = line:sub(rs1limit, rs2limit)
					rs2 = rs2:gsub(',', "")
					rs2 = rs2:gsub(' ', "")
					print("rs2 --------------- ".. rs2 .. " ----------------")
				end
				if type(replacment) == "string" then
					print("before replacing pseudo " .. replacment..rs1..rs2)
					replacment = replacment:gsub("rs1", rs1)
					replacment = replacment:gsub("rs2", rs2)
					print("replacing pseudo  with " .. replacment)
					print(replacment)
					instructions[#instructions+1] = replacment
					typetable[#typetable+1] = codetype
					counter = counter +1
					line = ""
				elseif type(replacment) == "table" then 
					print("--------------- "..i .. " ----------------")
					print("--------------- ".. replacment[1] .. " ----------------")
					print("rs2 --------------- ".. rs2 .. " ----------------")
					
					if (i == "LI")then
						temp = tonumber(rs2)
						if (temp >-2049) and (temp <2048)then
						print("use addi only not lui -------- ".. temp)
						replacment[1] = "nop"
						replacment[1] = "ADDI rs1,x0,rs2"
						end
					end
					for j ,replacment2 in ipairs(replacment) do
					print("------- replacment rs1 " .. rs1 .. " rs2 " .. rs2)
						replacment2 = replacment2:gsub("rs1", rs1)
						replacment2 = replacment2:gsub("rs2", rs2)
					print("replacment result----------- "..replacment2)
						instructions[#instructions+1] = replacment2
						typetable[#typetable+1] = codetype
						counter = counter +1
						line = ""
					end
				end
				return line
			end
		end
		for i ,replacment in pairs(pseudoinstructions3) do
			temph ='^' .. i .. '%s'
			templ ='^' .. i:lower() .. '%s'
			if (line:find(temph) ~= nil or line:find(templ) ~= nil) then
				print("found pseudo at " .. counter)
				oplength = line:find(" ")
				if oplength ~= nil then
					op = line:sub(1, oplength - 1)
					op = op:upper()
				end
				rs1limit = line:find(",", oplength)
				if rs1limit == nil  then
					errors[#errors+1] = " error at pseudo instruction at line " .. counter
				end
				if (rs1limit ~= nil and oplength ~=nil ) then
					rs1 = line:sub(oplength,rs1limit-1)
					rs1 = rs1:gsub(' ', "")
					rs1 = rs1:gsub(',', "")
				end
				rs2limit = line:find(",", oplength)
				if rs2limit == nil  then
					errors[#errors+1] = " error at pseudo instruction at line " .. counter
				end
				if (rs2limit ~= nil and oplength ~=nil ) then
					rs2 = line:sub(rs1limit, rs2limit-1)
					rs2 = rs2:gsub(',', "")
					rs2 = rs2:gsub(' ', "")
				end
				rs3limit = #line
				if (rs3limit ~= nil and oplength ~=nil ) then
					rs3 = line:sub(rs2limit, rs3limit)
					rs3 = rs3:gsub(',', "")
					rs3 = rs3:gsub(' ', "")
				end
				if type(replacment) == "string" then
					replacment = replacment:gsub("rs1", rs1)
					replacment = replacment:gsub("rs2", rs2)
					replacment2 = replacment:gsub("rs3", rs3)
					print("replacing pseudo ")
					print(replacment)
					instructions[#instructions+1] = replacment
					typetable[#typetable+1] = codetype
					counter = counter +1
					line = ""
				elseif type(replacment) == "table" then 
					for j ,replacment2 in ipairs(replacment) do
						replacment2 = replacment2:gsub("rs1", rs1)
						replacment2 = replacment2:gsub("rs2", rs2)
						replacment2 = replacment2:gsub("rs3", rs3)
						instructions[#instructions+1] = replacment2
						typetable[#typetable+1] = codetype
						counter = counter +1
						line = ""
					end
				end
				return line
			end
		end	
	end
	return line
end

function to_binary(number, length, signed)
	local out = ""
    local negative = nil
    local invert = nil
	local overflow_error = false
	if number < 0 then
		negative = true
		number = -number
		if  not signed then
			overflow_error = true
		end
	end
	while (number > 0) do
		out = number % 2 .. out
		number = math.floor(number / 2)
	end
	
	if ((#out > length) or (signed and (#out == length))) then
		overflow_error = true
	end

	while (#out < length) do
		out = 0 .. out
	end
	if negative then
		for i = length, 1, -1 do
			if out:sub(i,i) == "0" then
                if (invert) then
                    out = replace_char(i, out, "1")
                end
			else
                if (invert) then
                    out = replace_char(i, out, "0")
                end
                invert = true
			end
		end
	end
	return out, overflow_error
end

function writetofile(binarycodearray,outputfilename,filelimit)
	outputfile = io.open(outputfilename,"w")
	writelittleendian(binarycodearray,outputfile)
	nop = "00000000000000000000000000010011"
	insertins(counter,filelimit,nop,outputfile)
	io.close(outputfile)
end


function instobinary(instructions,outputfile)
	counter = 0
	local binarycodearray={}
	for i, line in ipairs(instructions) do
		print("this the line before turning to binary")
		print("line is " .. line)
		--print("type is" .. typetable[i] )
		if (typetable[i] == 2 ) then
			binarycodearray[#binarycodearray+1] = line
			goto nextloop
		end
		asmcode = ""
		oplength = line:find(" ")
		op = ""
		if oplength ~= nil then
			op = line:sub(1, oplength - 1)
			op = op:upper()
			print("extracted op and it is " .. op .. "------------")
			print("then what is it if not instruction? ------------" .. line )
		end
		if (R_TypeOps[op] ~= nil) then
			asmcode = R_TypeOps[op]
--------------------------------------------------------------
			rdlimit = line:find(",", oplength)
			if rdlimit == nil then
				errors[#errors+1] = "number of operands is too few at line " .. counter
				errors[#errors+1] = op
				print(" number of operands is too few " .. counter)
				os.exit()
			end
			rd = line:sub(oplength, rdlimit - 1)
			rd = rd:gsub(',', "")
			rd = rd:gsub(' ', "")
			rd,xcount = rd:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rd = tonumber(rd)
			if (tonumber(rd) ~= nil) then
				rd,overflow = to_binary(rd, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at first oprand rd at line " .. counter
				end
				asmcode = asmcode:gsub("rd", rd)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end
------------------------------------------------------------
			print(asmcode)
			if asmcode:find("rs2") == nil then
				rs1limit = #line
				print("second mode")
			else
				rs1limit = line:find(",", rdlimit + 1)
			end
			if rs1limit == nil   then
				errors[#errors+1] = "number of operands is too few " .. counter
				print(" number of operands is too few " .. counter)
				os.exit()
			end
			if asmcode:find("rs2") == nil then
				rs1 = line:sub(rdlimit, rs1limit )
			else
				rs1 = line:sub(rdlimit, rs1limit - 1)
			end
			rs1 = rs1:gsub(',', "")
			rs1 = rs1:gsub(' ', "")
			rs1,xcount = rs1:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rs1 = tonumber(rs1)
			if (tonumber(rs1) ~= nil) then 
				rs1,overflow = to_binary(rs1, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at second operand rs1 at line " .. counter
				end
				asmcode = asmcode:gsub("rs1", rs1)
				asmcode = asmcode:gsub("rm", rm)
				print(asmcode)
			end

			if (asmcode:find("rs2") ~= nil) then 
				print("found rs2")
				rs2limit = #line
				rs2 = line:sub(rs1limit, rs2limit)
				rs2 = rs2:gsub(',', "")
				rs2 = rs2:gsub(' ', "")
				rs2,xcount = rs2:gsub('[xX]', "")
				if xcount <1 then
					errors[#errors+1] = "you entered an immediate instead of a register at third operand at line ".. counter .." \n using ADDI instead " 
				--	use ADDI instead
					print("use ADDI instead ------------")
					imm = tonumber(rs2)
					asmcode = I_TypeOps["ADDI"]
					asmcode = asmcode:gsub("rd", rd)
					asmcode = asmcode:gsub("rs1", rs1)
					if (tonumber(imm) ~= nil) then
						imm,overflow = to_binary(imm, 12, false)
						if overflow then
							errors[#errors+1] = "overflow error at third operand rs2 at line " .. counter
						end
						asmcode = asmcode:gsub("imm%[11:0%]",imm)
						print(asmcode)
					else
						errors[#errors+1] = "you didn't enter a number in the immediate field at  " .. counter
					end
				else
					rs2 = tonumber(rs2)
					if (tonumber(rs2) ~= nil) then
						rs2,overflow = to_binary(rs2, 5, false)
						if overflow then
							errors[#errors+1] = "overflow error at third operand rs2 at line " .. counter
						end
						asmcode = asmcode:gsub("rs2", rs2)
						asmcode = asmcode:gsub("rm", rm)
						asmcode = asmcode:gsub("aq", aq)
						asmcode = asmcode:gsub("rl", rl)
						print(asmcode)
					else
						errors[#errors+1] = "you didn't enter a register number at line " .. counter
					end
				end
			end
			counter= counter +1
			print(counter)

		elseif (R4_TypeOps[op] ~= nil) then
			asmcode = R4_TypeOps[op]
     		 print(asmcode)
--------------------------------------------------------------
			rdlimit = line:find(",", oplength)
			if rdlimit == nil then
				errors[#errors+1] = "number of operands is too few at line " .. counter
				print("number of operands is too few at line  " .. counter)
				os.exit()
			end
			rd = line:sub(oplength, rdlimit - 1)
			rd = rd:gsub(',', "")
			rd = rd:gsub(' ', "")
			rd,xcount = rd:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rd = tonumber(rd)
			if (tonumber(rd) ~= nil) then
				rd,overflow = to_binary(rd, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at first oprand rd at line " .. counter
				end
				asmcode = asmcode:gsub("rd", rd)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end
------------------------------------------------------------
			rs1limit = line:find(",", rdlimit + 1)
			if rs1limit == nil then
				errors[#errors+1] = "number of operands is too few at line  " .. counter
				print("number of operands is too few at line  " .. counter)
				os.exit()
			end
			rs1 = line:sub(rdlimit, rs1limit - 1)
			rs1 = rs1:gsub(',', "")
			rs1 = rs1:gsub(' ', "")
			rs1,xcount = rs1:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rs1 = tonumber(rs1)
			if (tonumber(rs1) ~= nil) then
				rs1,overflow = to_binary(rs1, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at third operand rs1 at line " .. counter
				end
				asmcode = asmcode:gsub("rs1", rs1)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end

			rs2limit = line:find(",", rs1limit + 1)
			if rs2limit == nil then
				errors[#errors+1] = "number of operands is too few at line  " .. counter
				print("number of operands is too few at line  " .. counter)
				os.exit()
			end
			rs2 = line:sub(rs1limit, rs2limit-1)
			rs2 = rs2:gsub(',', "")
			rs2 = rs2:gsub(' ', "")
			rs2,xcount = rs2:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at third operand at line " .. counter
			end
			rs2 = tonumber(rs2)
			if (tonumber(rs2) ~= nil) then
				rs2,overflow = to_binary(rs2, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at third operand rs2 at line " .. counter
				end
				asmcode = asmcode:gsub("rs2", rs2)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end
			
			rs3limit = #line
			if rs3limit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("number of operands is too few " .. counter)
				os.exit()
			end
			rs3 = line:sub(rs2limit, rs3limit)
			rs3 = rs3:gsub(',', "")
			rs3 = rs3:gsub(' ', "")
			rs3,xcount = rs3:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at forth operand at line " .. counter
			end
			rs3 = tonumber(rs3)
			if (tonumber(rs3) ~= nil) then
				rs3,overflow = to_binary(rs3, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at third operand rs2 at line " .. counter
				end
				asmcode = asmcode:gsub("rs3", rs3)
				asmcode = asmcode:gsub("rm", rm)
				print(asmcode)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end
			counter= counter +1
			print(counter)

		elseif (U_TypeOps[op] ~= nil) then
			asmcode = U_TypeOps[op]
			print("this is U typr and op is ------ " .. op)
			rdlimit = line:find(",", oplength)
			if rdlimit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rd = line:sub(oplength, rdlimit -1 )
			rd = rd:gsub(',', "")
			rd = rd:gsub(' ', "")
			rd,xcount = rd:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rd = tonumber(rd)
			if (tonumber(rd) ~= nil) then
				rd,overflow = to_binary(rd, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at first oprand rd at line " .. counter
				end
				asmcode = asmcode:gsub("rd", rd)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end

			immlimit = #line
			imm = line:sub(rdlimit, immlimit )
			imm = imm:gsub(' ', "")
			imm,argcount = imm:gsub(',', "")
			if argcount  >1 then
				errors[#errors+1] ="number of operands is too many at line  " .. counter
			end
			
			if imm:sub(1,2) == "0x" then
				imm = imm:gsub('0x', "")
				imm,argcount2 = imm:gsub('[xX]', "")
				if argcount2 > 0 then
					errors[#errors+1] ="entered register instead of immediate at line " .. counter
				end
				imm = tonumber(imm,16)
			elseif imm:sub(1,2) == "0b" then
				imm = imm:gsub('0b', "")
				imm,argcount2 = imm:gsub('[xX]', "")
				if argcount2 > 0 then
					errors[#errors+1] ="entered register instead of immediate at line " .. counter
				end
				imm = tonumber(imm,2)
			else
				imm,argcount2 = imm:gsub('[xX]', "")
				if argcount2 > 0 then
					errors[#errors+1] ="entered register instead of immediate at line " .. counter
			end
			imm = tonumber(imm)
			end
			
			imm,overflow = to_binary(imm, 32, true)
			if overflow then
				errors[#errors+1]  = "overflow at immediate at line " .. counter .. " it should be only 32 bits"
			end
			-- a workaround for negative values
			imm2 = tonumber(imm:sub(1,20),2)
			print("imm2 20 bits before " .. imm:sub(1,20) )
			print("imm2 number before " .. imm2 )
			--imm:sub(21,21) == "1"
			--if imm:sub(21,21) == "1"then
			--	imm2 =imm2 +1
			--end
			imm2 =to_binary(imm2, 20, true)
			print("imm2 20 after adding  " .. imm2)
			print("imm2 number after adding  " .. tonumber(imm2,2))
			asmcode = asmcode:gsub("imm%[31:12%]", imm2)
			print(asmcode)
			counter= counter +1
			print(counter)
		
		elseif (SB_TypeOps[op] ~= nil) then
			asmcode = SB_TypeOps[op]
			--print(asmcode)
			rs1limit = line:find(",", oplength)
			if rs1limit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rs1 = line:sub(oplength, rs1limit - 1)
			rs1 = rs1:gsub(',', "")
			rs1 = rs1:gsub(' ', "")
			rs1,xcount = rs1:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rs1 = tonumber(rs1)
			if (tonumber(rs1) ~= nil) then 
				rs1,overflow = to_binary(rs1, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at first operand rs1 at line " .. counter
				end
				asmcode = asmcode:gsub("rs1", rs1)
			else
				errors[#errors+1] = "you didn't enter a register number at first operand at line " .. counter
			end

			rs2limit = line:find(",", rs1limit + 1)
			if rs2limit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rs2 = line:sub(rs1limit, rs2limit - 1)
			rs2 = rs2:gsub(',', "")
			rs2 = rs2:gsub(' ', "")
			rs2,xcount = rs2:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at second operand at line " .. counter
			end
			rs2 = tonumber(rs2)
			if (tonumber(rs2) ~= nil) then
				rs2,overflow = to_binary(rs2, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at second operand rs2 at line " .. counter
				end
				asmcode = asmcode:gsub("rs2", rs2)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end

			immlimit = #line
			imm = line:sub(rs2limit, immlimit )
			imm = imm:gsub(' ', "")
			imm,argcount = imm:gsub(',', "")
			if argcount  >1 then
				errors[#errors+1] ="number of operands is too many at line  " .. counter
			end
			if #imm ~= 0 then
				if imm:sub(1,2) == "0x" then
					imm = imm:gsub('0x', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,16)
				elseif imm:sub(1,2) == "0b" then
					imm = imm:gsub('0b', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,2)
				else 
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm)
				end
				if (tonumber(imm) ~= nil) then
					imm,overflow = to_binary(imm, 13, true)
					if overflow then
						errors[#errors+1]  = "warning overflow at immediate at line " .. counter .." it should be only 13 bits"
					end
					asmcode = asmcode:gsub("imm%[10:5%]", imm:sub(1,1) ..imm:sub(3,8))
					asmcode = asmcode:gsub("imm%[4:1%]", imm:sub(9,12).. imm:sub(2,2))

				else
						errors[#errors+1] = "you didn't enter a valid label at line " .. counter
				end
			else
				errors[#errors+1] = " you didn't enter a number in the immediate field at line " .. counter
			end
			print(asmcode)
			print(counter)
			counter= counter +1

		elseif (I_TypeOps[op] ~= nil) then
			print("entered I type " ..I_TypeOps[op] .. "  -----------------")
			print("OP is " .. op .. " -------------")
			print("line is " .. line)
			print("i is ".. i)
			asmcode = I_TypeOps[op]
			if op == "CSRRW" or op == "CSRRS" or op == "CSRRC" or op == "CSRRW" or op == "CSRRS" or op == "CSRRC" then 
				if op == "CSRRS" then
					rdlimit = line:find(",", oplength)
					if rdlimit == nil then
						errors[#errors+1] = "number of operands is too few " .. counter
						print("ERROR : number of operands is too few " .. counter)
						os.exit()
					end
					rd = line:sub(oplength, rdlimit - 1)
					rd = rd:gsub(',', "")
					rd = rd:gsub(' ', "")
					rd,xcount = rd:gsub('[xX]', "")
					if xcount <1 then
						errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
					end
					rd = tonumber(rd)
					if (tonumber(rd) ~= nil) then
						rd,overflow = to_binary(rd, 5, false)
						if overflow then
							errors[#errors+1] = "overflow error at first oprand rd at line " .. counter
						end
						asmcode = asmcode:gsub("rd", rd)
					else
						errors[#errors+1] = "you didn't enter a register number at line " .. counter
					end

					csrlimit = #line
					csr = line:sub(rdlimit, csrlimit )
					csr = csr:gsub(',', "")
					csr = csr:gsub(' ', "")
					csr = csr:gsub('[xX]', "")
					csrcode = csrcodes[csr]
					asmcode = asmcode:gsub("csr", csrcode)
					asmcode = asmcode:gsub("rs1", "00000")
				end

				if op == "CSRRW" or op == "CSRRC" then
					csrlimit = line:find(",", oplength)
					if csrlimit == nil then 
						errors[#errors+1] = "number of operands is too few " .. counter
						print("ERROR : number of operands is too few " .. counter)
						os.exit()
					end
					csr = line:sub(oplength, csrlimit - 1)
					csr = csr:gsub(',', "")
					csr = csr:gsub(' ', "")
					csr = csr:gsub('[xX]', "")
					csr = csr:gsub('[xX]', "")
					csr = csrcodes[csr]
					asmcode = asmcode:gsub("csr", csr)

					rs1limit = #line
					rs1 = line:sub(csrlimit, rs1limit )
					rs1 = rs1:gsub(',', "")
					rs1 = rs1:gsub(' ', "")
					rs1,xcount = rs1:gsub('[xX]', "")
					if xcount <1 then
						errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
					end
					rs1 = tonumber(rs1)
					if (tonumber(rs1) ~= nil) then
						rs1,overflow = to_binary(rs1, 5, false)
						if overflow then
							errors[#errors+1] = "overflow error at second operand rs1 at line " .. counter
						end
						asmcode = asmcode:gsub("rs1", rs1)
						asmcode = asmcode:gsub("rd", "00000")
					else
						errors[#errors+1] = "you didn't enter a register number at second operand rs1 at line " .. counter
					end
				end
				if op == "CSRRWI" or op == "CSRRSI" or op == "CSRRCI" then
					csrlimit = line:find(",", oplength)
					rd = line:sub(oplength, csrlimit - 1)
					rd = rd:gsub(',', "")
					rd = rd:gsub(' ', "")
					rd,xcount = rd:gsub('[xX]', "")
					if xcount <1 then
						errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
					end
					csr = csrcodes[csr]
					asmcode = asmcode:gsub("csr", csr)

					immlimit = #line
					imm = line:sub(csrlimit, immlimit )
					imm = imm:gsub(' ', "")
					imm,argcount = imm:gsub(',', "")
					if argcount  >1 then
						errors[#errors+1] ="number of operands is too many at line  " .. counter
					end
					if #imm ~=0 then
						if imm:sub(1,2) == "0x" then
							imm = imm:gsub('0x', "")
							imm,argcount2 = imm:gsub('[xX]', "")
							if argcount2 > 0 then
								errors[#errors+1] ="entered register instead of immediate at line " .. counter
							end
							imm = tonumber(imm,16)
						elseif imm:sub(1,2) == "0b" then
							imm = imm:gsub('0b', "")
							imm,argcount2 = imm:gsub('[xX]', "")
							if argcount2 > 0 then
								errors[#errors+1] ="entered register instead of immediate at line " .. counter
							end
							imm = tonumber(imm,2)
						else
							imm,argcount2 = imm:gsub('[xX]', "")
							if argcount2 > 0 then
								errors[#errors+1] ="entered register instead of immediate at line " .. counter
							end
							imm = tonumber(imm)
						end
						if (tonumber(imm) ~= nil) then
							imm,overflow = to_binary(imm, 5, true)
							if overflow then
								errors[#errors+1]  = "overflow at csr immediate at line " .. counter .. " it should be only 5 bits"
							end
							asmcode = asmcode:gsub("imm%[4:0%]", imm)
							asmcode = asmcode:gsub("rd", "00000")
						else
							errors[#errors+1] = "you didn't enter a valid label at line " .. counter
						end
					else
						errors[#errors+1] = " you didn't enter a number in the immediate field at line " .. counter
					end
				end
			goto continue
			end

			if op == "ECALL" or op == "EBREAK" or op == "URET" or op == "SRET" or op == "MRET" or op == "WFI"then 
				goto continue
			end
			if op == "LB" or op == "LH" or op == "LW" or op == "LD" or op == "LBU" or op == "LHU" or op == "LWU"     then 
				rdlimit = line:find(",", oplength)
				if rdlimit == nil then
					errors[#errors+1] = "number of operands is too few " .. counter
					print("ERROR : number of operands is too few " .. counter)
					os.exit()
				end
				rd = line:sub(oplength, rdlimit - 1)
				rd = rd:gsub(',', "")
				rd = rd:gsub(' ', "")
				rd,xcount = rd:gsub('[xX]', "")
				if xcount <1 then
					errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
				end
				rd = tonumber(rd)
				if (tonumber(rd) ~= nil) then
					rd,overflow = to_binary(rd, 5, false)
					if overflow then
					errors[#errors+1] = "overflow error at first oprand rd at line " .. counter
					end
					asmcode = asmcode:gsub("rd", rd)
				else
					errors[#errors+1] = "you didn't enter a register number at line " .. counter
				end

				rs1limit = #line
				if rs1limit == nil then
					errors[#errors+1] = "number of operands is too few " .. counter
					print("ERROR : number of operands is too few " .. counter)
					os.exit()
				end
				rs1imm = line:sub(rdlimit, rs1limit )
				rs1imm = rs1imm:gsub(',', "")
				rs1imm = rs1imm:gsub(' ', "")
				exp = "([-]?%d*)%((%a?)(%d*)"
				imm,out2,rs1= rs1imm:match(exp)
				rs1 = tonumber(rs1)
				if (tonumber(rs1) ~= nil) then
					rs1,overflow = to_binary(rs1, 5, false)
					if overflow then
					errors[#errors+1] = "overflow error at second operand rs2 at line " .. counter
					end
					asmcode = asmcode:gsub("rs1", rs1)
				else
					errors[#errors+1] = "you didn't enter a register number at second opearand  at line " .. counter
				end
				imm = imm:gsub(' ', "")
				imm = imm:gsub(' ', "")
				if imm ~= 0 then
					if imm:match("0x") ~= null then
					imm = imm:gsub('0x', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,16)
					elseif imm:match("0b") ~= null then
					imm = imm:gsub('0b', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,2)
					else
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm)
					end
					print("imm in i type " .. imm)
					--print(imm)
					if (tonumber(imm) ~= nil) then
						imm,overflow = to_binary(imm, 12, true)
						print("imm bin in i type " .. imm)
						if overflow then
							errors[#errors+1]  = "warning overflow at immediate at line " .. counter .. " it should be only 12 bits"
						end
						asmcode = asmcode:gsub("imm%[11:0%]", imm:sub((#imm - 11),#imm))
						asmcode = asmcode:gsub("imm%[4:0%]", imm:sub((#imm - 4),#imm))
						asmcode = asmcode:gsub("imm%[5:0%]", imm:sub((#imm - 5),#imm))
					else
					errors[#errors+1] = " you didn't enter a number in the immediate field at line " .. counter
					end
				end
				goto continue
			end
			rdlimit = line:find(",", oplength)
			if rdlimit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rd = line:sub(oplength, rdlimit - 1)
			rd = rd:gsub(',', "")
			rd = rd:gsub(' ', "")
			rd,xcount = rd:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rd = tonumber(rd)
			if (tonumber(rd) ~= nil) then
				rd,overflow = to_binary(rd, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at first oprand rd at line " .. counter
				end
				asmcode = asmcode:gsub("rd", rd)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end
			
			rs1limit = line:find(",", rdlimit + 1)
			if rs1limit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rs1 = line:sub(rdlimit, rs1limit - 1)
			rs1 = rs1:gsub(',', "")
			rs1 = rs1:gsub(' ', "")
			rs1,xcount = rs1:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line" .. counter
			end
			rs1 = tonumber(rs1)
			if (tonumber(rs1) ~= nil) then 
				rs1,overflow = to_binary(rs1, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at second operand rs1 at line " .. counter
				end
				asmcode = asmcode:gsub("rs1", rs1)
			else
				errors[#errors+1] = "you didn't enter a register number at second operand at line " .. counter
			end

			immlimit = #line
			imm = line:sub(rs1limit, immlimit )
			imm = imm:gsub(' ', "")
			imm,argcount = imm:gsub(',', "")
			if argcount  >1 then
				errors[#errors+1] ="number of operands is too many at line  " .. counter
			end
			if imm ~= 0 then
				if imm:sub(1,2) == "0x" then
					imm = imm:gsub('0x', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,16)
				elseif imm:sub(1,2) == "0b" then
					imm = imm:gsub('0b', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,2)
				else
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm)
				end
				print("imm in i type " .. imm)
					--print(imm)
				if (tonumber(imm) ~= nil) then
					imm,overflow = to_binary(imm, 12, true)
					print("imm bin in i type " .. imm)
					if overflow then
						errors[#errors+1]  = "warning overflow at immediate at line " .. counter .. " it should be only 12 bits"
					end
					asmcode = asmcode:gsub("imm%[11:0%]", imm:sub((#imm - 11),#imm))
					asmcode = asmcode:gsub("imm%[4:0%]", imm:sub((#imm - 4),#imm))
					asmcode = asmcode:gsub("imm%[5:0%]", imm:sub((#imm - 5),#imm))
				else
					errors[#errors+1] = "you didn't enter a valid label at line " .. counter
				end
			else
				errors[#errors+1] = " you didn't enter a number in the immediate field at line " .. counter
			end
			::continue::
			print(asmcode)
			print(counter)
			counter= counter +1

		elseif (UJ_TypeOps[op] ~= nil) then
			asmcode = UJ_TypeOps[op]

			rdlimit = line:find(",", oplength)
			if rdlimit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rd = line:sub(oplength, rdlimit - 1)
			rd = rd:gsub(',', "")
			rd = rd:gsub(' ', "")
			rd,xcount = rd:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rd = tonumber(rd)
			if (tonumber(rd) ~= nil) then
				rd,overflow = to_binary(rd, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at first oprand rd at line " .. counter
				end
				asmcode = asmcode:gsub("rd", rd)
			else
				errors[#errors+1] = "you didn't enter a register number at line " .. counter
			end

			immlimit = #line
			imm = line:sub(rdlimit, immlimit )
			imm = imm:gsub(' ', "")
			imm,argcount = imm:gsub(',', "")
			if argcount  >1 then
				errors[#errors+1] ="number of operands is too many at line  " .. counter
			end
			if #imm ~= 0 then
				if imm:sub(1,2) == "0x" then
					imm = imm:gsub('0x', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,16)
				elseif imm:sub(1,2) == "0b" then
					imm = imm:gsub('0b', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,2)
				else
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm)
				end
				if (tonumber(imm) ~= nil) then
					imm,overflow = to_binary(imm, 21, true)
					if overflow then
						errors[#errors+1]  = "warning overflow at immediate at line " .. counter .. " it should be only 21 bits"
					end
					asmcode = asmcode:gsub("imm", imm:sub(1,1) .. imm:sub(11,20) .. imm:sub(10,10) .. imm:sub(2,9))
				else
							errors[#errors+1] = "you didn't enter a valid label at line " .. counter
					end
			else
				errors[#errors+1] = " you didn't enter a number in the immediate field at line " .. counter
			end
			print(asmcode)
			print(counter)
			counter= counter +1
		
		
		elseif (S_TypeOps[op] ~= nil) then
			asmcode = S_TypeOps[op]
			
			rs2limit = line:find(",", oplength)
			if rs2limit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rs2 = line:sub(oplength, rs2limit - 1)
			rs2 = rs2:gsub(',', "")
			rs2 = rs2:gsub(' ', "")
			rs2,xcount = rs2:gsub('[xX]', "")
			if xcount <1 then
				errors[#errors+1] = "you entered an immediate instead of a register at line " .. counter
			end
			rs2 = tonumber(rs2)
			if (tonumber(rs2) ~= nil) then
				rs2,overflow = to_binary(rs2, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at first operand rs1 at line " .. counter
				end
				asmcode = asmcode:gsub("rs2", rs2)
			else
				errors[#errors+1] = "you didn't enter a register number  at first operand at line " .. counter
			end

			rs1limit = #line
			if rs1limit == nil then
				errors[#errors+1] = "number of operands is too few " .. counter
				print("ERROR : number of operands is too few " .. counter)
				os.exit()
			end
			rs1imm = line:sub(rs2limit, rs1limit - 1)
			rs1imm = rs1imm:gsub(',', "")
			rs1imm = rs1imm:gsub(' ', "")
			exp = "([-]?%d*)%((%a?)(%d*)"
			imm,out2,rs1= rs1imm:match(exp)
			rs1 = tonumber(rs1)
			if (tonumber(rs1) ~= nil) then
				rs1,overflow = to_binary(rs1, 5, false)
				if overflow then
					errors[#errors+1] = "overflow error at second operand rs2 at line " .. counter
				end
				asmcode = asmcode:gsub("rs1", rs1)
			else
				errors[#errors+1] = "you didn't enter a register number at second opearand  at line " .. counter
			end
			imm = imm:gsub(' ', "")
			imm,argcount = imm:gsub(',', "")
			imm = imm:gsub(' ', "")
			if #imm ~= 0 then
				if imm:match("0x") ~= null then
					imm = imm:gsub('0x', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,16)
				elseif  imm:match("0b") ~= null then
					imm = imm:gsub('0b', "")
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm,2)
				else
					imm,argcount2 = imm:gsub('[xX]', "")
					if argcount2 > 0 then
						errors[#errors+1] ="entered register instead of immediate at line " .. counter
					end
					imm = tonumber(imm)
				end
				if (tonumber(imm) ~= nil) then
					imm,overflow = to_binary(imm, 12, true)
					if overflow  then
						errors[#errors+1]  = "warning overflow at immediate at line " .. counter .. " it should be only 12 bits"
					end
					asmcode = asmcode:gsub("imm%[4:0%]", imm:sub(8,12))
					asmcode = asmcode:gsub("imm%[11:5%]", imm:sub(1,7))
				else
					errors[#errors+1] = "you didn't enter a valid label at line " .. counter
				end
			else
				errors[#errors+1] = " you didn't enter a number in the immediate field at line " .. counter
			end

			print(asmcode)
			print(counter)
			counter= counter +1
		end
		if #asmcode ~= 0 then
			binarycodearray[#binarycodearray+1] = asmcode
		elseif #line ~= 0 then 
			--binarycodearray[#binarycodearray+1] = line
		end
		::nextloop::
		--writelittleendian(asmcode,outputfile)
	end
	return binarycodearray,counter
end
-----------------------------------
--initialization
errors = {}
typetable = {}
commentchar = ";"
print("start getting lables numbers")
counter = 0
labels = {}
instructions = {}
--------------------------------
if (arg[1] ~= nil) then
asmfile = io.open(arg[1])
else
asmfile = io.open("instructions.txt")
print("No excution arguments so using the default instructions.txt")
end
------------------------
instructions , labels =  extractins(asmfile)
print( "number of instructions ".. #instructions )
instructions= replacelabels(instructions, labels)
binarycodearray,counter = instobinary(instructions)
if outputfilename == nil then
	outputfilename = "binary.txt"
end
filelimit =256
writetofile(binarycodearray,outputfilename,filelimit)
print("Errors and Warnings")
for i,instructionn in ipairs(instructions) do 
	print(instructionn)
end
for i,errorline in ipairs(errors) do 
	print(errorline)
end
if #errors == 0 then
	print("nothing")
end
--print("typetable")
--for key,value in pairs(binarycodearray) do
--print(key .. " --- " ..value)
--end
print(counter)
--[[
if counter < 128 then 
    for i =1,(128-counter) do
      	for i=25, 0,-8 do 
        	outputfile:write(asmcode:sub(i,i+7),"\n") 
		end
    end
end]]--

--[[if (line:find(":") ~= nil) then
	labellength = line:find(":")
	label  = line:sub(1, labellength - 1)
	label = label:gsub(' ', "")
	if (tonumber(label:sub(1,1)) == nil) or (label:find(",") ~= nil) then
		if (counter == 0) then
			labels[label] = 0
		else
			labels[label] = (counter+1)*4
		end
	else
		errors[#errors+1] = "you entered a label that starts with a number or has a , in it at line " .. counter
	end
	possibleins = line:sub(labellength+1,#line)
	if #possibleins~=0 then
		if (isins(possibleins) ) then
			--counter = counter+1
			--instructions[#instructions+1] = possibleins
			line = possibleins
		else
			print(" ERROR : you entered something not instruction after : ")
			errors[#errors+1] = "you entered something not instruction after :at line " .. counter
			print("at line " .. counter)
		end
	end
end--]]
--[[if (line:find(commentchar) ~= nil) then
	commentstart = line:find(commentchar)
	possibleins  = line:sub(1,commentstart-1)
	if  #possibleins~=0 then
		if (isins(possibleins)  ) then
			--counter = counter +1
			--instructions[#instructions+1] = possibleins
			line = possibleins
		else
			print("ERROR: you wrote something not instruction before the ; ")
			print("at line " .. counter)
			errors[#errors+1] = "you wrote something not instruction before the ; at line  " .. counter
		end
	end
end--]]
-- print labels tables
--for key,value in pairs(labels) do 
--	print(key .. " "..value)
--end
-- replace labels with there values

--[[
asmfile = io.open("instructions.txt")
tempfile = io.open("temp.txt","w")
line = asmfile:read("*line")
while (line ~= nil  ) do
	if ((line:find(":") == nil) and line:find(commentchar) == nil and (#line ~= 0) ) then
		for key,value in pairs(labels) do
			if (line:find(key) ~= nil ) then 
				print(line)
				print("label value " .. value)
				print("current instruction address " .. (counter+1)*4)
				print("replace label with ".. value-((counter+1)*4))
				line = line:gsub(key,value-((counter+1)*4))
				print(line)
			end
		end
		counter = counter+1
	end
	tempfile:write(line,"\n")
	line = asmfile:read("*line")
end
asmfile:close()
tempfile:close()

--]]

-- trim instructions
--[[
for i,subject in ipairs(instructions) do
  instructions[i]= trim(subject)
	print(instructions[i])
end
--]]
-- crate a table of labels

--[[
asmfile = io.open("instructions.txt")
line = asmfile:read("*line")
while (line ~= nil  ) do
	if (line:find(":") ~= nil and (line:find(commentchar) == nil)) then
			labellength = line:find(":")
			label  = line:sub(1, labellength - 1)
			label = label:gsub(' ', "")
			if (counter == 0) then
				labels[label] = 0
			else
				labels[label] = (counter+1)*4
			end
	elseif (#line ~= 0) then 
			counter = counter +1
	end

	line = asmfile:read("*line")
end
asmfile:close()
--]]
-- initialize the Errors array

--replace labels
--[[
-- replace labels with the difference between the instruction address and label address
for i, instruction in ipairs(instructions) do
	for key,value in pairs(labels) do
		if (instruction:find(key) ~= nil ) then 
			print(instruction)
			print("label value " .. value)
			print("current instruction address " .. i*4)
			print("replace label with ".. value-(i*4))
			instruction = instruction:gsub(key,value-(i*4))
			instructions[i] = instruction
			print(instruction)
		end
	end
end
--]]