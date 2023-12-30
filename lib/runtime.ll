@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d   = internal constant [4 x i8] c"%d \00"	
@lf  = internal constant [4 x i8] c"%lf\00"
@str.error = internal constant [15 x i8] c"runtime error\0A\00"	
@stdin = external global i8

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare i8* @gets(i8*)
declare void @exit(i32)
declare i8* @malloc(i32)
declare i32 @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare i32 @strcmp(i8*, i8*)
declare i8* @readline()


define void @fun.printInt(i32 %x) {
  %r0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %r0, i32 %x) 
  ret void
}

define i32 @fun.readInt() {
  %res = alloca i32
  %r1 = getelementptr [4 x i8], [4 x i8]* @d, i32 0, i32 0
  call i32 (i8*, ...) @scanf(i8* %r1, i32* %res)
  %r2 = load i32, i32* %res
  ret i32 %r2
}

define void @fun.printString(i8* %str) {
  call i32 @puts(i8* %str)
	ret void
}

define i8* @fun.readString() {
  %r1 = call i8* @readline()
  ret i8* %r1
}

define i8* @fun.internal.concatStrings(i8* %str1, i8* %str2) {
  %r1 = call i32 @strlen(i8* %str1)
  %r2 = call i32 @strlen(i8* %str2)
  %r3 = add i32 %r1, 1
  %r4 = add i32 %r3, %r2
  %r5 = call i8* @malloc(i32 %r4)
  %r6 = call i8* @strcpy(i8* %r5, i8* %str1)
  %r7 = call i8* @strcat(i8* %r6, i8* %str2)
  ret i8* %r7
}

define i1 @fun.internal.compareStrings(i8* %str1, i8* %str2) {
  %1 = call i32 @strcmp(i8* %str1, i8* %str2)
  %2 = icmp eq i32 %1, 0
  ret i1 %2
}

define void @fun.error() {
  %r0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str.error, i64 0, i64 0))
  call void @exit(i32 1)
  unreachable
}