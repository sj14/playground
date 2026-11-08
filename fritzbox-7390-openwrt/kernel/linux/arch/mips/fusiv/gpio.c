// SPDX-License-Identifier: GPL-2.0
/*
 * GPIO bank of the Fusiv Vx180, 16 lines with 16 bit registers.
 *
 * The layout comes from vx180.h in AVM's GPL sources: a 1 in the direction
 * register makes a line an output, the flag register reads the lines, and
 * writing a 1 to the clear or set half word drives one line low or high.
 */

#include <linux/gpio/driver.h>
#include <linux/gpio/generic.h>
#include <linux/init.h>
#include <linux/platform_device.h>

#define GPIO_DIRECTION	0x0
#define GPIO_FLAG	0x4
#define GPIO_CLEAR	0x4
#define GPIO_SET	0x6

static int fusiv_gpio_probe(struct platform_device *pdev)
{
	struct device *dev = &pdev->dev;
	struct gpio_generic_chip_config config;
	struct gpio_generic_chip *chip;
	void __iomem *base;
	int ret;

	chip = devm_kzalloc(dev, sizeof(*chip), GFP_KERNEL);
	if (!chip)
		return -ENOMEM;

	base = devm_platform_ioremap_resource(pdev, 0);
	if (IS_ERR(base))
		return PTR_ERR(base);

	config = (struct gpio_generic_chip_config) {
		.dev = dev,
		.sz = 2,
		.dat = base + GPIO_FLAG,
		.set = base + GPIO_SET,
		.clr = base + GPIO_CLEAR,
		.dirout = base + GPIO_DIRECTION,
	};
	ret = gpio_generic_chip_init(chip, &config);
	if (ret)
		return dev_err_probe(dev, ret, "cannot set up the GPIO chip\n");

	return devm_gpiochip_add_data(dev, &chip->gc, NULL);
}

static const struct of_device_id fusiv_gpio_of_match[] = {
	{ .compatible = "ikanos,vx180-gpio" },
	{ }
};

static struct platform_driver fusiv_gpio_driver = {
	.probe = fusiv_gpio_probe,
	.driver = {
		.name = "fusiv-gpio",
		.of_match_table = fusiv_gpio_of_match,
	},
};
builtin_platform_driver(fusiv_gpio_driver);
