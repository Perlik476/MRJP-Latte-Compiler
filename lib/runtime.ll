@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d   = internal constant [4 x i8] c"%d \00"	
@lf  = internal constant [4 x i8] c"%lf\00"
@str.error = internal constant [15 x i8] c"runtime error\0A\00"	
@stdin = external global i8

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i8* @gets(i8*)
declare void @exit(i32)
declare i8* @malloc(i32)
declare i32 @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare i32 @strcmp(i8*, i8*)
declare i32 @_printString(i8*)
declare i8* @_readString()
declare i32 @_compareStrings(i8*, i8*)
declare i8* @_concatStrings(i8*, i8*)
declare i32 @_readInt()


define void @fun.printInt(i32 %x) {
  %r0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %r0, i32 %x) 
  ret void
}

define i32 @fun.readInt() {
  %r1 = call i32 @_readInt()
  ret i32 %r1
}

define void @fun.printString(i8* %str) {
  call i32 @_printString(i8* %str)
	ret void
}

define i8* @fun.readString() {
  %r1 = call i8* @_readString()
  ret i8* %r1
}

define i8* @fun.internal.concatStrings(i8* %str1, i8* %str2) {
  %r1 = call i8* @_concatStrings(i8* %str1, i8* %str2)
  ret i8* %r1
}

define i1 @fun.internal.compareStrings(i8* %str1, i8* %str2) {
  %r1 = call i32 @_compareStrings(i8* %str1, i8* %str2)
  %r2 = icmp eq i32 %r1, 1
  ret i1 %r2
}

define void @fun.error() {
  %r0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.error, i64 0, i64 0))
  call void @exit(i32 134)
  unreachable
}