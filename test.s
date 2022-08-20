	.data
	.align 2
_x:	.space 4
	.text
	.globl main
main:		#  METHOD ENTRY
__start:		#  add __start label for main only
	sw    $ra, 0($sp)
	subu  $sp, $sp, 4
	sw    $ra, 0($sp)
	subu  $sp, $sp, 4
	addu  $fp, $sp, 8
	sw    $ra, 0($sp)
	subu  $sp, $sp, 4
	sw    $fp, 0($sp)
	subu  $sp, $sp, 4
	addu  $fp, $sp, 8
	subu  $sp, $sp, 4
	li    $t0, 1
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	addu  $t1, $fp, -8
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	addu  $t1, $fp, -8
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t0, 0($t0)
	beq   $t0, 1, .L0
	b     .L1
.L0:
	li    $t0, 3
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	addu  $t1, $fp, -12
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	li    $t0, 1
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	la    $t1, _x
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	addu  $t1, $fp, -8
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $a0, 0($t0)
	li    $v0, 1
	syscall
	la    $t1, _x
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t0, 0($t0)
	beq   $t0, 1, .L2
	b     .L3
.L2:
	li    $t0, 2
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	addu  $t1, $fp, -12
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	addu  $t1, $fp, -12
	sw    $t1, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $a0, 0($t0)
	li    $v0, 1
	syscall
.L3:
.L1:
	lw    $v0, 4($sp)	# POP
	addu  $sp, $sp, 4
_main_exit:
	lw    $ra, 0($fp)
	move  $t0, $fp
	lw    $fp, -4($fp)
	move  $sp, $t0
	li    $v0, 10
	syscall
